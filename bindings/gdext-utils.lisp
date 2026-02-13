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
(defun initialize-interface (get-proc-address-ptr)
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
   (parameters :initarg :parameters :reader parameters-of)))


(defclass extension-class ()
  ((bind-name :initarg :bind :reader bind-of)
   (method-table :initform (make-hash-table :test 'eq)
                 :reader method-table-of)
   (builtin-p :initform nil
              :initarg :builtin
              :reader builtinp)
   (variant-kind :initform nil :reader variant-kind-of)))


(defmethod initialize-instance :after ((this extension-class) &key builtin)
  (with-slots (variant-kind bind-name) this
    (setf
     variant-kind
     (if builtin
         (uiop:if-let ((kind (cdr (assoc bind-name *variant-name-kind-mapping*
                                         :test #'string=))))
           kind
           (error "Unknown Builtin type ~A" bind-name))
         :object))))


(defun register-extension-class (name bind &key builtin)
  (unless (gethash name *extension-registry*)
    (setf
     (gethash name *extension-registry*)
     (make-instance 'extension-class
                    :bind bind
                    :builtin builtin))))


(defun get-extension-class (name)
  (uiop:if-let ((extension (gethash name *extension-registry*)))
    extension
    (error "Extension class ~A not found" name)))


(defun register-extension-method (class-name method-name hash bind-name bind-var)
  (setf
   (gethash method-name (method-table-of (get-extension-class class-name)))
   (make-instance 'extension-method
                  :bind bind-name
                  :hash hash
                  :bind-var bind-var)))


(defun get-extension-method (class-name method-name)
  (uiop:if-let ((extension (gethash class-name *extension-registry*)))
    (uiop:if-let ((method (gethash method-name (method-table-of extension))))
      method
      (error "Method ~A not found in extension clas ~A" method-name class-name))
    (error "Extension class ~A not found" class-name)))


(defmacro defgclass (name-and-opts &body properties)
  (declare (ignore properties))
  (destructuring-bind (name &key bind builtin) (uiop:ensure-list name-and-opts)
    `(progn
       (cffi:defctype ,name (:pointer :void))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (register-extension-class ',name ,bind :builtin ,builtin)))))


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
           (bind (eval bind))
           (ptr-var-name (format-symbol-into '%%gdext.util~secret "*~A~~~A*"
                                             'extension-method lisp-name))
           (instance-var (format-symbol-into '%%gdext.util~secret "~A" 'class-instance))
           (params (loop for (name) in arguments
                         collect name)))
      `(progn
         (defvar ,ptr-var-name (cffi:null-pointer))
         (defun ,lisp-name (,@(unless static
                                (list instance-var))
                            ,@params)
           (,(if (builtinp (get-extension-class class-name))
                 'call-builtin-method
                 'call-method-bind)
            (get-extension-method ',class-name ',lisp-name)
            ,ptr-var-name ,(if static
                               (cffi:null-pointer)
                               instance-var)
            ,@params))
         (register-extension-method ',class-name ',lisp-name
                                    ,hash ,bind ',ptr-var-name)))))


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


(defun call-builtin-method (method method-ptr object &rest args)
  (cffi:with-foreign-object (argv :pointer (length args))
    (loop for arg in args
          for i from 0
          do (setf (cffi:mem-ref argv :pointer i) arg))
    (cffi:with-foreign-object (ret :pointer)
      (funcall-prototype method-ptr %gdext.types:ptr-built-in-method
                         object
                         argv
                         ret
                         (length args))
      ret)))


(defun get-method-bind (class-name method-name hash)
  (with-godot-string-names ((class-string-name class-name)
                            (method-string-name method-name))
    (%gdext.interface:classdb-get-method-bind class-string-name
                                              method-string-name
                                              hash)))


(defun call-method-bind (method method-bind object &rest args)
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


(defun initialize-extension (extension-class)
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
