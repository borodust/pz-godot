(cl:in-package :%gdext.util)

;; Godot Extension API doesn't provide alignment explicitly, but
;; Variant's union alignment declared as alignas(8) in the code
(a:define-constant +default-alignment+ 8 :test #'=)

(defvar *godot-class-registry* (make-hash-table :test 'eq))

(defvar *godot-singleton-registry* (make-hash-table :test 'eq))

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
    ("PackedVector4Array" . :packed-vector4-array)

    ("Variant" . :variant)))

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
(defclass godot-method ()
  ((bind-name :initarg :bind :reader bind-of)
   (hash :initarg :hash :reader hash-of)
   (return-type :initarg :return-type :reader return-type-of)
   (parameter-types :initarg :parameter-types :reader parameter-types-of)
   (vararg-p :initarg :vararg :reader varargp)
   (virtual-p :initarg :virtual :reader virtualp)))


(defclass godot-constructor ()
  ((index :initarg :index :reader index-of)
   (parameter-types :initarg :parameter-types :reader parameter-types-of)))


(defclass godot-class ()
  ((bind-name :initarg :bind :reader bind-of)
   (method-table :initform (make-hash-table :test 'eq)
                 :reader method-table-of)
   (constructor-table :initform (make-hash-table :test 'eql)
                      :reader constructor-table-of)
   (has-destructor-p :initform nil
                     :reader has-destructor-p
                     :writer %set-has-destructor)
   (api :initarg :api :reader api-of)
   (variant-kind :initarg :variant-kind :reader variant-kind-of)
   (size :initarg :size :reader size-of)
   (primitive :initform nil :reader primitivep)))


(defun builtinp (instance)
  (eq :builtin (api-of instance)))


(defmethod initialize-instance :after ((this godot-class) &key)
  (with-slots (variant-kind bind-name primitive) this
    (setf primitive (and (member variant-kind '(:float :int :bool)) t))))


(defun register-godot-class (name bind variant-kind &rest keys &key api &allow-other-keys)
  (unless (gethash name *godot-class-registry*)
    (setf
     (gethash name *godot-class-registry*)
     (apply #'make-instance 'godot-class
            :bind bind
            :variant-kind variant-kind
            keys))
    (pushnew name (gethash api *godot-class-registry*))))


(defun get-godot-class (name &key (error-if-not-found t))
  (uiop:if-let ((extension (gethash name *godot-class-registry*)))
    extension
    (when error-if-not-found
      (error "Godot class ~A not found" name))))


(defun register-godot-method (class-name method-name hash bind-name
                                  &rest keys &key &allow-other-keys)
  (setf
   (gethash method-name (method-table-of (get-godot-class class-name)))
   (apply #'make-instance 'godot-method
          :bind bind-name
          :hash hash
          keys)))


(defun register-godot-constructor (class-name index &rest keys &key &allow-other-keys)
  (setf
   (gethash index (constructor-table-of (get-godot-class class-name)))
   (apply #'make-instance 'godot-constructor
          :index index
          keys)))


(defun register-godot-destructor (class-name)
  (%set-has-destructor t (get-godot-class class-name)))


(defun get-godot-method (class-name method-name)
  (uiop:if-let ((extension (gethash class-name *godot-class-registry*)))
    (uiop:if-let ((method (gethash method-name (method-table-of extension))))
      method
      (error "Method ~A not found in godot class ~A" method-name class-name))
    (error "Godot class ~A not found" class-name)))


(defun get-godot-constructor (class-name index)
  (uiop:if-let ((extension (gethash class-name *godot-class-registry*)))
    (uiop:if-let ((ctor (gethash index (constructor-table-of extension))))
      ctor
      (error "Constructor ~A not found in godot class ~A" index class-name))
    (error "Godot class ~A not found" class-name)))


(defun calc-arg-stack-layout (types)
  (loop with cur-offset = 0
        for type in types
        for type-alignment = (method-argument-type-stack-alignment type)
        for type-size = (method-argument-type-stack-size type)
        for padding = (if (zerop type-size)
                          0
                          (mod (- type-alignment (mod cur-offset type-alignment)) type-alignment))
        unless (zerop padding)
          append (list :pad padding) into layout
        append (list :value type-size) into layout
        do (incf cur-offset (+ padding type-size))
        finally (return (values layout cur-offset))))


(defmacro with-method-argv ((ptr-var arg-count-var &rest args) &body body)
  (multiple-value-bind (argc vals types)
      (loop for (type val) on args by #'cddr
            collect (eval type) into types
            collect val into vals
            finally (return (values (length vals) vals types)))
    (multiple-value-bind (layout stack-size)
        (calc-arg-stack-layout types)
      (a:with-gensyms (arg-data-ptr arg-data argv-ptr stack-ptr)
        `(let ((,arg-data (make-array ,(+
                                        ;; arg ptr vector size
                                        argc
                                        ;; arg stack size
                                        (ceiling stack-size +default-alignment+))
                                      :element-type '(signed-byte ,(* +default-alignment+ 8)))))
           (declare (dynamic-extent ,arg-data))
           (cffi:with-pointer-to-vector-data (,arg-data-ptr ,arg-data)
             ,@(loop with stack-byte-offset = (* +default-alignment+ argc)
                     and argv-byte-offset = 0
                     and arg-idx = 0
                     for (mem-kind bytes) on layout by #'cddr
                     when (eq mem-kind :value)
                       collect (let ((type (nth arg-idx types))
                                     (val (nth arg-idx vals)))
                                 `(let ((,argv-ptr (cffi:inc-pointer ,arg-data-ptr ,argv-byte-offset))
                                        (,stack-ptr ,(if (zerop bytes)
                                                         '(cffi:null-pointer)
                                                         `(cffi:inc-pointer ,arg-data-ptr ,stack-byte-offset))))
                                    ,(expand-method-argument-translation type val argv-ptr stack-ptr)))
                       and do (incf arg-idx)
                     do (incf stack-byte-offset bytes)
                        (incf argv-byte-offset +default-alignment+))
             (let ((,ptr-var ,arg-data-ptr)
                   (,arg-count-var ,argc))
               ,@body)))))))


(defmacro with-method-varargs ((ptr-var arg-count-var args) &body body)
  (a:with-gensyms (arg-data)
    (a:once-only (args)
      `(let* ((,arg-count-var (length ,args))
              (,arg-data (make-array ,arg-count-var
                                     :element-type '(signed-byte ,(* +default-alignment+ 8)))))
         (declare (dynamic-extent ,arg-data))
         (cffi:with-pointer-to-vector-data (,ptr-var ,arg-data)
           (loop for ptr in ,args
                 for i from 0
                 do (setf (cffi:mem-aref ,ptr-var :pointer i) ptr))
           ,@body)))))


(defmacro defgclass (name-and-opts &body properties)
  (destructuring-bind (name &key bind api size) (uiop:ensure-list name-and-opts)
    (let* ((size (eval size))
           (api (eval api))
           (bind (eval bind))
           (builtin (eq :builtin api))
           (variant-kind (if builtin
                             (uiop:if-let ((kind (a:assoc-value *variant-name-kind-mapping* bind :test #'string=)))
                               kind
                               (error "Unknown Builtin type ~A" bind))
                             :object)))
      `(progn
         ,@(if (and builtin
                    (not (member bind '("float" "int" "bool" "Nil") :test #'equal)))
               (progn
                 (assert (= +default-alignment+ 8))
                 (a:if-let ((fields (a:assoc-value properties :fields)))
                   `((cffi:defcstruct (,name :size ,size)
                       ,@(loop for field in fields
                               collect (destructuring-bind (name type &key offset)
                                           field
                                         (list name type :offset offset))))
                     (cffi:defctype ,name (:struct ,name)))
                   ;; we use %%align field to force whole union alignment to +default-alignment+ value
                   ;; if no fields known
                   `((cffi:defcunion ,name
                       (%%data :int8 :count ,size)
                       (%%align :int64))
                     (cffi:defctype ,name (:union ,name)))))
               `((cffi:defctype ,name ,(a:switch (bind :test #'equal)
                                         ("Nil" :void)
                                         ("float" (assert (= size 8))
                                                  :double)
                                         ("int" (assert (= size 8))
                                                :int64)
                                         ("bool" (assert (= size 1))
                                                 :bool)
                                         (t '(:pointer :void))))))

         (eval-when (:compile-toplevel)
           ,@(a:switch (bind :test #'equal)
               ("Nil" (list))
               ("float"
                (loop for type in (list :float :double name)
                      append `((defmethod method-argument-type-stack-alignment ((type (eql ',type)))
                                 (cffi:foreign-type-alignment :double))
                               (defmethod method-argument-type-stack-size ((type (eql ',type)))
                                 (cffi:foreign-type-size :double))
                               (defmethod expand-method-argument-translation ((type (eql ',type))
                                                                              src-val-sym
                                                                              argv-ptr-sym
                                                                              stack-ptr-sym)
                                 (expand-ptrarg-with-conversion
                                  :double src-val-sym argv-ptr-sym stack-ptr-sym)))))

               ("int"
                (loop for type in (list :uint8 :int8 :uint16 :int16 :uint32 :int32 :int64 :uint64
                                        :int :size 'wchar name)
                      append `((defmethod method-argument-type-stack-alignment ((type (eql ',type)))
                                 (cffi:foreign-type-alignment :int64))
                               (defmethod method-argument-type-stack-size ((type (eql ',type)))
                                 (cffi:foreign-type-size :int64))
                               (defmethod expand-method-argument-translation ((type (eql ',type))
                                                                              src-val-sym
                                                                              argv-ptr-sym
                                                                              stack-ptr-sym)
                                 (expand-ptrarg-with-conversion
                                  :int64 src-val-sym argv-ptr-sym stack-ptr-sym)))))
               ("bool"
                (loop for type in (list :bool name)
                      append `((defmethod method-argument-type-stack-alignment ((type (eql ',type)))
                                 (cffi:foreign-type-alignment :uint8))
                               (defmethod method-argument-type-stack-size ((type (eql ',type)))
                                 (cffi:foreign-type-size :uint8))
                               (defmethod expand-method-argument-translation ((type (eql ',type))
                                                                              src-val-sym
                                                                              argv-ptr-sym
                                                                              stack-ptr-sym)
                                 (expand-ptrarg-with-conversion
                                  :uint8 src-val-sym argv-ptr-sym stack-ptr-sym)))))
               (t (if builtin
                      `((defmethod method-argument-type-stack-alignment ((type (eql ',name)))
                          (cffi:foreign-type-alignment ',name))
                        (defmethod method-argument-type-stack-size ((type (eql ',name)))
                          (cffi:foreign-type-size ',name))
                        (defmethod expand-method-argument-translation ((type (eql ',name))
                                                                       src-val-sym
                                                                       argv-ptr-sym
                                                                       stack-ptr-sym)
                          (expand-ptrarg-blob ',name src-val-sym argv-ptr-sym stack-ptr-sym)))

                      `((defmethod method-argument-type-stack-alignment ((type (eql ',name)))
                          0)
                        (defmethod method-argument-type-stack-size ((type (eql ',name)))
                          0)
                        (defmethod expand-method-argument-translation ((type (eql ',name))
                                                                       src-val-sym
                                                                       argv-ptr-sym
                                                                       stack-ptr-sym)
                          (expand-ptrarg-pointer src-val-sym argv-ptr-sym stack-ptr-sym)))))))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (register-godot-class ',name ,bind
                                 ,variant-kind
                                 :api ,api
                                 :size ,(if (or (not size)
                                                (= size 0))
                                            0
                                            `(cffi:foreign-type-size ',name))))))))


(defun register-godot-singleton (name class-name)
  (let ((class (get-godot-class class-name)))
    (setf (gethash name *godot-singleton-registry*) (bind-of class))))


(defmacro defgsingleton (name class-name)
  (let* ((class-name (eval class-name))
         (extension (get-godot-class class-name)))
    (a:with-gensyms (instance-var)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (register-godot-singleton ',name ',class-name))
         (defun ,name ()
           (let ((,instance-var (get-singleton ,(bind-of extension))))
             (funcall (compile ',name (lambda () ,instance-var)))))))))


(defmacro defgenum (name-and-opts &body values)
  (destructuring-bind (name &key class ((:bitfield bitfield-p)))
      (uiop:ensure-list name-and-opts)
    (declare (ignore class))
    `(progn
       (,(if bitfield-p 'cffi:defbitfield 'cffi:defcenum)  ,name
        ,@values)

       (eval-when (:compile-toplevel)
         (defmethod method-argument-type-stack-alignment ((type (eql ',name)))
           (cffi:foreign-type-alignment :int64))
         (defmethod method-argument-type-stack-size ((type (eql ',name)))
           (cffi:foreign-type-size :int64))
         (defmethod expand-method-argument-translation ((type (eql ',name))
                                                        src-val-sym
                                                        argv-ptr-sym
                                                        stack-ptr-sym)
           (expand-ptrarg-with-conversion
            :int64 src-val-sym argv-ptr-sym stack-ptr-sym))))))


(defmacro defgmethod (name-and-opts return-type &body arguments)
  (destructuring-bind (lisp-name &key bind ((:class class-name)) hash static vararg virtual)
      (uiop:ensure-list name-and-opts)
    (let* ((class-name (eval class-name))
           (bind (eval bind))
           (extension (get-godot-class class-name))
           (instance-var (format-symbol-into '%%gdext.util~secret "~A" 'instance))
           (ret-var (format-symbol-into '%%gdext.util~secret "~A" 'result) ))
      (multiple-value-bind (params param-types)
          (loop for (name type) in arguments
                collect name into names
                collect type into types
                finally (return (values names types)))
        (a:with-gensyms (fun-ptr-var)
          (let* ((lambda-list `(,@(unless static
                                    (list instance-var))
                                ,@(unless (eq return-type :void)
                                    (list ret-var))
                                ,@params))
                 (bind-extractor (if (builtinp extension)
                                     `(get-builtin-method ,(variant-kind-of extension)
                                                          ,bind
                                                          ,hash)
                                     `(get-method-bind ,(bind-of extension)
                                                       ,bind
                                                       ,hash))))
            `(progn
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (register-godot-method ',class-name ',lisp-name
                                        ,hash ,bind
                                        :parameter-types ',param-types
                                        :return-type ',return-type
                                        :vararg ,vararg
                                        :virtual ,virtual))
               ,(if vararg
                    `(defun ,lisp-name (,@lambda-list &rest variants)
                       (let ((,fun-ptr-var ,bind-extractor))
                         (when (cffi:null-pointer-p ,fun-ptr-var)
                           (error "Failed to get method pointer with ~A" ',bind-extractor))
                         (apply (compile ',lisp-name
                                         (lambda (,@lambda-list
                                                  &rest variants)
                                           (call-vararg-method-bind
                                            ,fun-ptr-var ,(if static
                                                              (cffi:null-pointer)
                                                              instance-var)
                                            ,(unless (eq return-type :void)
                                               ret-var)
                                            (list* ,@params variants))))
                                ,@lambda-list variants)))
                    `(defun ,lisp-name (,@lambda-list)
                       (let ((,fun-ptr-var ,bind-extractor))
                         (when (cffi:null-pointer-p ,fun-ptr-var)
                           (error "Failed to get method pointer with ~A" ',bind-extractor))
                         (funcall (compile ',lisp-name
                                           (lambda (,@lambda-list)
                                             (,(if (builtinp extension)
                                                   'call-builtin-method
                                                   'ptrcall-method-bind)
                                              ,class-name ,lisp-name
                                              ,fun-ptr-var ,(if static
                                                                (cffi:null-pointer)
                                                                instance-var)
                                              ,(unless (eq return-type :void)
                                                 ret-var)
                                              ,@params)))
                                  ,@lambda-list)))))))))))


(defmacro defgconstructor (name-and-opts &body arguments)
  (destructuring-bind (lisp-name &key ((:class class-name)) index)
      (uiop:ensure-list name-and-opts)
    (let* ((class-name (eval class-name))
           (extension (get-godot-class class-name))
           (index (eval index))
           (instance-var (format-symbol-into '%%gdext.util~secret "~A" 'instance)))
      (multiple-value-bind (params param-types)
          (loop for (name type) in arguments
                collect name into names
                collect type into types
                finally (return (values names types)))
        (a:with-gensyms (ptr-var)
          `(progn
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (register-godot-constructor ',class-name ,index
                                           :parameter-types ',param-types))
             (defun ,lisp-name (,instance-var ,@params)
               (let ((,ptr-var (get-constructor-bind ,(variant-kind-of extension) ,index)))
                 (funcall
                  (compile ',lisp-name
                           (lambda (,instance-var ,@params)
                             (call-builtin-constructor ,class-name ,index ,ptr-var ,instance-var ,@params)
                             ,instance-var))
                  ,instance-var ,@params)))))))))


(defmacro defgdestructor (name-and-opts)
  (destructuring-bind (lisp-name &key ((:class class-name)))
      (uiop:ensure-list name-and-opts)
    (let* ((class-name (eval class-name))
           (extension (get-godot-class class-name))
           (instance-var (format-symbol-into '%%gdext.util~secret "~A" 'instance)))
      (a:with-gensyms (dtor-ptr-var)
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (register-godot-destructor ',class-name))
           (defun ,lisp-name (,instance-var)
             (let ((,dtor-ptr-var (%gdext.interface:variant-get-ptr-destructor ,(variant-kind-of extension))))
               (funcall
                (compile ',lisp-name
                         (lambda (,instance-var)
                           (call-builtin-destructor ,dtor-ptr-var ,instance-var)))
                ,instance-var))))))))


(defmacro defgproperty (lisp-name class-name &body keys &key index get set)
  (declare (ignore keys))
  (let* ((class-name (eval class-name))
         (get (eval get))
         (set (eval set))
         (index (eval index))
         (instance-var (format-symbol-into '%%gdext.util~secret "~A" 'instance))
         (value-var (format-symbol-into '%%gdext.util~secret "~A" 'value))
         (result-var (format-symbol-into '%%gdext.util~secret "~A" 'result))
         (setter-returns-p (when set
                             (let ((method (get-godot-method class-name set)))
                               (not (eq (return-type-of method) :void))))))
    `(progn
       ,@(when get
           `((defun ,lisp-name (,instance-var ,result-var)
               (,get ,instance-var ,result-var ,@(when index (list index)))
               ,result-var)))
       ,@(when set
           `((defun (setf ,lisp-name) (,instance-var
                                       ,@(when setter-returns-p
                                           (list result-var))
                                       ,value-var)
               (,set ,instance-var
                     ,@(when setter-returns-p
                         (list result-var))
                     ,@(when index (list index))
                     ,value-var)
               ,value-var))))))


(defmacro with-godot-alloc ((var size) &body body)
  `(let ((,var (%gdext.interface:mem-alloc2 ,size 0)))
     (unwind-protect
          (progn ,@body)
       (%gdext.interface:mem-free2 ,var 0))))


;;;
;;; GODOT STRING
;;;
(cffi:defcunion godot-string
  (%%data :uint64))


(defun init-string (ptr lisp-string)
  (cffi:with-foreign-string (content-ptr lisp-string :encoding :utf-8)
    (%gdext.interface:string-new-with-utf8-chars ptr content-ptr)))


(export 'destroy-string)
(defvar *string-destructor-ptr* (cffi:null-pointer))
(defun destroy-string (ptr)
  (when (cffi:null-pointer-p *string-destructor-ptr*)
    (setf *string-destructor-ptr* (%gdext.interface:variant-get-ptr-destructor :string)))
  (funcall-prototype *string-destructor-ptr* %gdext.types:ptr-destructor ptr))


(export 'with-godot-string)
(defmacro with-godot-string ((var lisp-string) &body body)
  `(with-godot-alloc (,var (cffi:foreign-type-size '(:union godot-string)))
     (init-string ,var ,lisp-string)
     (unwind-protect
          (progn ,@body)
       (destroy-string ,var))))


(export 'with-godot-strings)
(defmacro with-godot-strings (bindings &body body)
  (if bindings
      (destructuring-bind (var lisp-string) (first bindings)
        `(with-godot-string (,var ,lisp-string)
           (with-godot-strings ,(rest bindings)
             ,@body)))
      `(progn ,@body)))


(export 'godot-string-to-lisp)
(defun godot-string-to-lisp (godot-string)
  (let ((length (%gdext.interface:string-to-utf8-chars godot-string
                                                       (cffi:null-pointer)
                                                       0)))

    (cref:c-with ((foreign-string :char :count (1+ length)))
      (%gdext.interface:string-to-utf8-chars godot-string
                                             (foreign-string &)
                                             length)
      (setf (foreign-string length) 0)
      (cffi:foreign-string-to-lisp (foreign-string &)
                                   :encoding :utf-8
                                   :count length))))


;;;
;;; GODOT STRING NAME
;;;
(cffi:defcunion string-name
  (%%data :uint64))


(defun init-string-name (ptr lisp-string)
  (cffi:with-foreign-string (content-ptr lisp-string :encoding :utf-8)
    (%gdext.interface:string-name-new-with-utf8-chars ptr content-ptr)))


(export 'destroy-string-name)
(defvar *string-name-destructor-ptr* (cffi:null-pointer))
(defun destroy-string-name (ptr)
  (when (cffi:null-pointer-p *string-name-destructor-ptr*)
    (setf *string-name-destructor-ptr* (%gdext.interface:variant-get-ptr-destructor :string-name)))
  (funcall-prototype *string-name-destructor-ptr* %gdext.types:ptr-destructor ptr))


(export 'with-godot-string-name)
(defmacro with-godot-string-name ((var lisp-string) &body body)
  `(with-godot-alloc (,var (cffi:foreign-type-size '(:union string-name)))
     (init-string-name ,var ,lisp-string)
     (unwind-protect
          (progn ,@body)
       (destroy-string-name ,var))))


(export 'with-godot-string-names)
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
  (let* ((method (get-godot-method class-name method-name))
         (param-types (parameter-types-of method)))
    (a:with-gensyms (argv argc)
      `(with-method-argv (,argv ,argc ,@(loop for param-type in param-types
                                              for arg in args
                                              append `(',param-type ,arg)))
         (funcall-prototype ,method-ptr %gdext.types:ptr-built-in-method
                            ,object
                            ,argv
                            ,(if ret ret '(cffi:null-pointer))
                            ,argc)
         ,(when ret ret)))))


(defmacro call-builtin-constructor (class-name index ctor-ptr instance-ptr &rest args)
  (let* ((ctor (get-godot-constructor class-name index))
         (param-types (parameter-types-of ctor)))
    (a:with-gensyms (argv argc)
      `(with-method-argv (,argv ,argc ,@(loop for param-type in param-types
                                              for arg in args
                                              append `(',param-type ,arg)))
         (declare (ignore ,argc))
         (funcall-prototype ,ctor-ptr %gdext.types:ptr-constructor
                            ,instance-ptr
                            ,argv)))))


(defmacro call-builtin-destructor (dtor-ptr instance-ptr)
  `(funcall-prototype ,dtor-ptr %gdext.types:ptr-destructor ,instance-ptr))


(defun get-method-bind (class-name method-name hash)
  (with-godot-string-names ((class-string-name class-name)
                            (method-string-name method-name))
    (%gdext.interface:classdb-get-method-bind class-string-name
                                              method-string-name
                                              hash)))


(defun get-constructor-bind (variant-type index)
  (%gdext.interface:variant-get-ptr-constructor variant-type index))


(defun get-singleton (name)
  (with-godot-string-names ((string-name name))
    (%gdext.interface:global-get-singleton string-name)))


(defmacro ptrcall-method-bind (class-name method-name method-bind object ret
                            &rest args)
  (let* ((method (get-godot-method class-name method-name))
         (param-types (parameter-types-of method)))
    (a:with-gensyms (argv argc)
      `(with-method-argv (,argv ,argc ,@(loop for param-type in param-types
                                              for arg in args
                                              append `(',param-type ,arg)))
         (declare (ignore ,argc))
         (%gdext.interface:object-method-bind-ptrcall
          ,method-bind
          ,object
          ,argv
          ,(if ret ret '(cffi:null-pointer)))
         ,(when ret ret)))))


(defmacro call-vararg-method-bind (method-bind object ret variants)
  (a:once-only (variants)
    (a:with-gensyms (argv argc)
      `(with-method-varargs (,argv ,argc ,variants)
         (%gdext.interface:object-method-bind-call
          ,method-bind
          ,object
          ,argv
          ,argc
          ,(if ret ret '(cffi:null-pointer))
          (cffi:null-pointer))
         ,(when ret ret)))))


;;;
;;; METADATA
;;;
;
; Don't make these generic functions.
; The compilation/loading performance is atrocious (especially for multidispatch one)
;
(defun godot-extension-bind-name (class-name)
  (bind-of (get-godot-class class-name)))

(defun godot-extension-variant-kind (class-name)
  (variant-kind-of (get-godot-class class-name)))

(defun godot-extension-method-bind-name (class-name method-name)
  (bind-of (get-godot-method class-name method-name)))
