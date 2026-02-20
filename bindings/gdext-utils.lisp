(cl:in-package :%gdext.util)


(defvar *extension-registry* (make-hash-table :test 'eq))

(defvar *variant-name-kind-mapping*
  '(("Nil"                . :nil)
    ("bool"               . :bool)
    ("int"                . :int)
    ("float"              . :float)
    ("String"             . :string)
    ("Vector2"            . :vector2)
    ("Vector2i"           . :vector2i)
    ("Rect2"              . :rect2)
    ("Rect2i"             . :rect2i)
    ("Vector3"            . :vector3)
    ("Vector3i"           . :vector3i)
    ("Transform2D"        . :transform2d)
    ("Vector4"            . :vector4)
    ("Vector4i"           . :vector4i)
    ("Plane"              . :plane)
    ("Quaternion"         . :quaternion)
    ("AABB"               . :aabb)
    ("Basis"              . :basis)
    ("Transform3D"        . :transform3d)
    ("Projection"         . :projection)
    ("Color"              . :color)
    ("StringName"         . :string-name)
    ("NodePath"           . :node-path)
    ("RID"                . :rid)
    ("Callable"           . :callable)
    ("Signal"             . :signal)
    ("Dictionary"         . :dictionary)
    ("Array"              . :array)
    ("PackedByteArray"    . :packed-byte-array)
    ("PackedInt32Array"   . :packed-int32-array)
    ("PackedInt64Array"   . :packed-int64-array)
    ("PackedFloat32Array" . :packed-float32-array)
    ("PackedFloat64Array" . :packed-float64-array)
    ("PackedStringArray"  . :packed-string-array)
    ("PackedVector2Array" . :packed-vector2-array)
    ("PackedVector3Array" . :packed-vector3-array)
    ("PackedColorArray"   . :packed-color-array)
    ("PackedVector4Array" . :packed-vector4-array)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INTERFACE
;;
(defun bind-interface (get-proc-address-ptr)
  (flet ((%get-proc-addr (c-name)
           (cffi:with-foreign-string (c-name-ptr c-name :encoding :latin1)
             (funcall-prototype get-proc-address-ptr
                                %gdext.types:interface-get-proc-address
                                c-name-ptr))))
    (loop for (c-name . ptr-var-name) being the hash-value in *interface-registry*
          do (setf (symbol-value ptr-var-name) (%get-proc-addr c-name))))
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EXTENSIONS
;;
(defclass extension-method ()
  ((bind-name :initarg :bind :reader bind-of)
   (hash :initarg :hash :reader hash-of)
   (bind-var :initarg :bind-var :reader bind-var-of)
   (return-type :initarg :return-type :reader return-type-of)
   (parameter-types :initarg :parameter-types :reader parameter-types-of)))


(defclass extension-class ()
  ((bind-name :initarg :bind :reader bind-of)
   (method-table :initform (make-hash-table :test 'eq)
                 :reader method-table-of)
   (builtin-p :initform nil
              :initarg :builtin
              :reader builtinp)
   (variant-kind :initform nil :reader variant-kind-of)
   (size :initarg :size :reader size-of)
   (primitive :initform nil :reader primitivep)))


(defmethod initialize-instance :after ((this extension-class) &key builtin)
  (with-slots (variant-kind bind-name primitive) this
    (setf
     variant-kind
     (if builtin
         (uiop:if-let ((kind (cdr (assoc bind-name *variant-name-kind-mapping*
                                         :test #'string=))))
           kind
           (error "Unknown Builtin type ~A" bind-name))
         :object))

    (setf primitive (and (member variant-kind '(:float :int :bool)) t))))


(defun register-extension-class (name bind &rest keys &key &allow-other-keys)
  (unless (gethash name *extension-registry*)
    (setf
     (gethash name *extension-registry*)
     (apply #'make-instance 'extension-class
            :bind bind
            keys))))


(defun get-extension-class (name)
  (uiop:if-let ((extension (gethash name *extension-registry*)))
    extension
    (error "Extension class ~A not found" name)))


(defun register-extension-method (class-name method-name hash bind-name bind-var
                                  &rest keys &key &allow-other-keys)
  (setf
   (gethash method-name (method-table-of (get-extension-class class-name)))
   (apply #'make-instance 'extension-method
          :bind bind-name
          :hash hash
          :bind-var bind-var
          keys)))


(defun get-extension-method (class-name method-name)
  (uiop:if-let ((extension (gethash class-name *extension-registry*)))
    (uiop:if-let ((method (gethash method-name (method-table-of extension))))
      method
      (error "Method ~A not found in extension class ~A" method-name class-name))
    (error "Extension class ~A not found" class-name)))


(cffi:defcfun ("memcpy" memcpy) :pointer
  (dst :pointer)
  (src :pointer)
  (size :size))


(defun calc-memory-layout (alignment types)
  (loop with cur-offset = 0
        for type in types
        for type-size = (size-of type)
        for padding = (mod (- alignment (mod cur-offset alignment)) alignment)
        unless (zerop padding)
          append (list :pad padding) into layout
        append (list :value type-size) into layout
        do (incf cur-offset (+ padding type-size))
        finally (return (values layout cur-offset))))


(defgeneric expand-type-translation (type value ptr)
  (:method (type value ptr)
    (a:once-only (ptr)
      `(memcpy ,ptr ,value ,(size-of type)))))


(defmacro with-method-argv ((ptr-var arg-count-var &rest args) &body body)
  (let ((alignment (cffi:foreign-type-alignment :pointer))
        (size (cffi:foreign-type-size :pointer)))
    (multiple-value-bind (argc vals types)
        (loop for (type val) on args by #'cddr
              collect (get-extension-class (eval type)) into types
              collect val into vals
              finally (return (values (length vals) vals types)))
      (multiple-value-bind (layout total-size)
          (calc-memory-layout alignment types)
        (a:with-gensyms (ptr array arg-ptr)
          `(let ((,array (make-array ,(+ argc (ceiling total-size size))
                                     :element-type '(signed-byte ,(* size 8)))))
             (declare (dynamic-extent ,array))
             (cffi:with-pointer-to-vector-data (,ptr ,array)
               ,@(loop with offset = 0
                       and idx = 0
                       for (mem-kind bytes) on layout by #'cddr
                       when (eq mem-kind :value)
                         collect (let ((type (nth idx types))
                                       (val (nth idx vals)))
                                   `(let ((,arg-ptr (cffi:inc-pointer ,ptr ,offset)))
                                      (setf (cffi:mem-aref ,ptr :pointer ,idx) ,arg-ptr)
                                      ,(expand-type-translation type val arg-ptr)))
                         and do (incf idx)
                       do (incf offset bytes))
               (let ((,ptr-var ,ptr)
                     (,arg-count-var ,argc))
                 ,@body))))))))


(defmacro defgclass (name-and-opts &body properties)
  (declare (ignore properties))
  (destructuring-bind (name &key bind builtin size) (uiop:ensure-list name-and-opts)
    (let ((size (eval size)))
     `(progn
        (cffi:defctype ,name ,(a:switch (bind :test #'equal)
                                ("float" (assert (= size 8))
                                         :double)
                                ("int" (assert (= size 8))
                                       :int64)
                                ("bool" (assert (= size 1))
                                        :bool)
                                (t '(:pointer :void))) )
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (register-extension-class ',name ,bind
                                    :builtin ,builtin
                                    :size ,size))))))


(defmacro defgenum (name-and-opts &body values)
  (destructuring-bind (name &key class ((:bitfield bitfield-p)))
      (uiop:ensure-list name-and-opts)
    (declare (ignore class))
   `(,(if bitfield-p 'cffi:defbitfield 'cffi:defcenum)  ,name
     ,@values)))


(defmacro defgmethod (name-and-opts return-type &body arguments)
  (destructuring-bind (lisp-name &key bind ((:class class-name)) hash static)
      (uiop:ensure-list name-and-opts)
    (let* ((class-name (eval class-name))
           (extension (get-extension-class class-name))
           (bind (eval bind))
           (ptr-var-name (format-symbol-into '%%gdext.util~secret "*~A~~~A*"
                                             'extension-method lisp-name))
           (instance-var (format-symbol-into '%%gdext.util~secret "~A" 'class-instance))
           (ret-class (unless (eq :void return-type)
                        (get-extension-class return-type)))
           (ret-var (format-symbol-into '%%gdext.util~secret "~A" 'result) ))
      (multiple-value-bind (params param-types)
          (loop for (name type) in arguments
                collect name into names
                collect type into types
                finally (return (values names types)))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (register-extension-method ',class-name ',lisp-name
                                        ,hash ,bind ',ptr-var-name
                                        :parameter-types ',param-types
                                        :return-type ',return-type))
           (defvar ,ptr-var-name (cffi:null-pointer))
           (defun ,lisp-name (,@(unless static
                                  (list instance-var))
                              ,@(when (and ret-class
                                           (not (primitivep ret-class)))
                                  (list ret-var))
                              ,@params)
             (,(if (builtinp (get-extension-class class-name))
                   'call-builtin-method
                   'call-method-bind)
              ,class-name ,lisp-name
              ,ptr-var-name ,(if static
                                 (cffi:null-pointer)
                                 instance-var)
              ,(when (and ret-class
                          (not (primitivep ret-class)))
                 ret-var)
              ,@params)))))))


(cffi:defcstruct (string-name :size 8))


(defmacro with-godot-alloc ((var size) &body body)
  `(let ((,var (%gdext.interface:mem-alloc ,size)))
     (unwind-protect
          (progn ,@body)
       (%gdext.interface:mem-free ,var))))


(defmacro with-godot-string-name ((var lisp-string) &body body)
  (let ((string-ptr (gensym)))
    `(with-godot-alloc (,var (cffi:foreign-type-size '(:struct string-name)))
       (cffi:with-foreign-string (,string-ptr ,lisp-string :encoding :utf-8)
         (%gdext.interface:string-name-new-with-utf8-chars ,var ,string-ptr)
         ,@body))))


(defmacro with-godot-string-names (bindings &body body)
  (if bindings
      (destructuring-bind (var lisp-string) (first bindings)
        `(with-godot-string-name (,var ,lisp-string)
           (with-godot-string-names ,(rest bindings)
             ,@body)))
      `(progn ,@body)))


(defun get-builtin-method (variant-kind method-name hash)
  (with-godot-string-names ((method-string-name method-name))
    (%gdext.interface:variant-get-ptr-builtin-method variant-kind
                                                     method-string-name
                                                     hash)))


(defmacro call-builtin-method (class-name method-name method-ptr object ret
                               &rest args)
  (let* ((method (get-extension-method class-name method-name))
         (param-types (parameter-types-of method))
         (return-type (return-type-of method)))
    (a:with-gensyms (argv argc)
      `(with-method-argv (,argv ,argc ,@(loop for param-type in param-types
                                              for arg in args
                                              append `(',param-type ,arg))
                                ,@(when ret
                                    `(',return-type ,ret)))
         (funcall-prototype ,method-ptr %gdext.types:ptr-built-in-method
                            ,object
                            ,argv
                            ,@(if ret
                                  `((cffi:inc-pointer
                                     ,argv (* ,(cffi:foreign-type-size :pointer)
                                              (1- ,argc)))
                                    (1- ,argc))
                                  `((cffi:null-pointer)
                                    ,argc)))))))


(defun get-method-bind (class-name method-name hash)
  (with-godot-string-names ((class-string-name class-name)
                            (method-string-name method-name))
    (%gdext.interface:classdb-get-method-bind class-string-name
                                              method-string-name
                                              hash)))


(defmacro call-method-bind (class-name method-name method-bind object ret
                            &rest args)
  (cffi:with-foreign-object (argv :pointer (length args))
    (loop for arg in args
          for i from 0
          do (setf (cffi:mem-ref argv :pointer i) arg))
    (cffi:with-foreign-object (ret :pointer)
      (%gdext.interface:object-method-bind-ptrcall method-bind
                                                   object
                                                   argv
                                                   ret)
      ret)))


(defun bind-extension (extension-class)
  (let ((extension (get-extension-class extension-class)))
    (loop for method being the hash-value in (method-table-of extension)
          for bind = (if (builtinp extension)
                         (get-builtin-method (variant-kind-of extension)
                                             (bind-of method)
                                             (hash-of method))
                         (get-method-bind (bind-of extension)
                                          (bind-of method)
                                          (hash-of method)))
          do (setf (symbol-value (bind-var-of method)) bind))))
