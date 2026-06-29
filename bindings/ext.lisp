(cl:defpackage :%godot.util
  (:use :cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:cref #:cffi-c-ref))
  (:import-from #:%gdext.util
                #:funcall-prototype
                #:do-interface
                #:memcpy)
  (:export #:bind-gdext-interface
           #:bind-godot-constants

           #:defgconstant
           #:defgenum
           #:defgclass
           #:defgconstructor
           #:defgdestructor
           #:defgproperty
           #:defgmethod
           #:defgsingleton

           #:method-argument-type-stack-alignment
           #:method-argument-type-stack-size
           #:expand-method-argument-translation

           #:godot-extension-bind-name
           #:godot-extension-variant-kind
           #:godot-extension-method-bind-name

           #:godot-string-to-lisp

           #:init-godot-string
           #:destroy-godot-string
           #:with-godot-string
           #:with-godot-strings

           #:init-godot-string-name
           #:destroy-godot-string-name
           #:with-godot-string-name
           #:with-godot-string-names))
(cl:defpackage :%%godot.util~secret
  (:use))
(cl:in-package :%godot.util)

;; Godot Extension API doesn't provide alignment explicitly, but
;; Variant's union alignment declared as alignas(8) in the code
(a:define-constant +default-alignment+ 8 :test #'=)

(defvar *godot-class-registry* (make-hash-table :test 'eq))

(defvar *godot-builtin-map* (make-hash-table :test 'eq))

(defvar *godot-singleton-registry* (make-hash-table :test 'eq))

(defvar *godot-builtin-constant-registry* (make-hash-table :test 'eq))

(defvar *godot-method-argument-metadata* (make-hash-table :test 'eq))

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
(defun bind-gdext-interface (get-proc-address-ptr)
  (flet ((%get-proc-addr (c-name)
           (cffi:with-foreign-string (c-name-ptr c-name :encoding :latin1)
             (funcall-prototype get-proc-address-ptr
                                %gdext:interface-get-proc-address
                                c-name-ptr))))
    (do-interface (c-name ptr)
      (setf ptr (%get-proc-addr c-name))))
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PTRCALL
;;
(defclass godot-method-argument-metadata ()
  ((size :initarg :size
         :reader size-of)
   (alignment :initarg :alignment
              :reader alignment-of)
   (translation-expander :initarg :translation-expander
                         :reader translation-expander-of)))


(defun register-godot-method-argument-metadata (type &rest keys
                                                &key &allow-other-keys)
  (setf
   (gethash type *godot-method-argument-metadata*)
   (apply #'make-instance 'godot-method-argument-metadata keys)))


(defun method-argument-type-stack-alignment (type)
  (a:if-let ((metadata (gethash type *godot-method-argument-metadata*)))
    (alignment-of metadata)
    (if (and (listp type)
             (eq :pointer (cffi::canonicalize-foreign-type type)))
        (method-argument-type-stack-alignment :pointer)
        (error "Failed to get method argument's alignment for argv stack: unrecognized type ~A" type))))


(defun method-argument-type-stack-size (type)
  (a:if-let ((metadata (gethash type *godot-method-argument-metadata*)))
    (size-of metadata)
    (if (and (listp type)
             (eq :pointer (cffi::canonicalize-foreign-type type)))
        (method-argument-type-stack-size :pointer)
        (error "Failed to get method argument size for argv stack: unrecognized type ~A" type))))


(defun expand-method-argument-translation (type src-val-sym argv-ptr-sym stack-ptr-sym)
  (a:if-let ((metadata (gethash type *godot-method-argument-metadata*)))
    (funcall (translation-expander-of metadata)
             src-val-sym argv-ptr-sym stack-ptr-sym)
    (if (and (listp type)
             (eq :pointer (cffi::canonicalize-foreign-type type)))
        (expand-method-argument-translation :pointer
                                            src-val-sym argv-ptr-sym stack-ptr-sym)
        (error "Failed to expand method argument translation: unrecognized type ~A" type))))


(defun expand-ptrarg-with-conversion (arg-type-to src-val-sym argv-ptr-sym stack-ptr-sym)
  `(setf (cffi:mem-ref ,argv-ptr-sym :pointer) ,stack-ptr-sym
         (cffi:mem-ref ,stack-ptr-sym ',arg-type-to) ,(a:switch (arg-type-to)
                                                        (:int64 `(truncate ,src-val-sym))
                                                        (:double `(float ,src-val-sym 0d0))
                                                        (t src-val-sym))))


(defun expand-ptrarg-enum (enum bitfield-p src-val-sym argv-ptr-sym stack-ptr-sym)
  `(setf (cffi:mem-ref ,argv-ptr-sym :pointer) ,stack-ptr-sym
         (cffi:mem-ref ,stack-ptr-sym :int64) ,(cond
                                                 ((integerp src-val-sym) src-val-sym)
                                                 ((and (listp src-val-sym) (eq 'quote (first src-val-sym)))
                                                  (cffi:foreign-bitfield-value enum (second src-val-sym)))
                                                 ((keywordp src-val-sym)
                                                  (if bitfield-p
                                                      (cffi:foreign-bitfield-value enum src-val-sym)
                                                      (cffi:foreign-enum-value enum src-val-sym)))
                                                 (t (if bitfield-p
                                                        `(cffi:foreign-bitfield-value ',enum ,src-val-sym)
                                                        `(cffi:foreign-enum-value ',enum ,src-val-sym))))))


(defun expand-ptrarg-blob (arg-type src-val-sym argv-ptr-sym stack-ptr-sym)
  `(progn
     (setf (cffi:mem-ref ,argv-ptr-sym :pointer) ,stack-ptr-sym)
     (memcpy ,stack-ptr-sym ,src-val-sym (cffi:foreign-type-size ',arg-type))))


(defun expand-ptrarg-pointer (src-val-sym argv-ptr-sym stack-ptr-sym)
  `(setf (cffi:mem-ref ,argv-ptr-sym :pointer) ,stack-ptr-sym
         (cffi:mem-ref ,stack-ptr-sym :pointer) ,src-val-sym))


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
    (unless (eq :object variant-kind)
      (setf (gethash variant-kind *godot-builtin-map*) name))
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


(defun get-godot-builtin (kind &key (error-if-not-found t))
  (a:if-let ((class-name (gethash kind *godot-builtin-map*)))
    (get-godot-class class-name :error-if-not-found error-if-not-found)
    (error "Class not found for builtin of type ~A" kind)))


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


(defun register-godot-builtin-constant (constant-name &key initializer)
  (setf (gethash constant-name *godot-builtin-constant-registry*) initializer)
  (values))


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
                              (incf argv-byte-offset +default-alignment+)
                     do (incf stack-byte-offset bytes))
             (let ((,ptr-var ,arg-data-ptr)
                   (,arg-count-var ,argc))
               ,@body)))))))


(defmacro with-method-varargs ((ptr-var arg-count-var &rest args-and-rest) &body body)
  (a:with-gensyms (arg-data)
    (destructuring-bind (rest &rest reverse-args) (reverse args-and-rest)
      (let ((args (reverse reverse-args)))
        `(let* ((,arg-count-var (+ ,(length args) (length ,rest)))
                (,arg-data (make-array ,arg-count-var
                                       :element-type '(signed-byte ,(* +default-alignment+ 8)))))
           (declare (dynamic-extent ,arg-data))
           (cffi:with-pointer-to-vector-data (,ptr-var ,arg-data)
             ,@(loop for ptr in args
                     for i from 0
                     collect `(setf (cffi:mem-aref ,ptr-var :pointer ,i) ,ptr))
             (when ,rest
               (loop for ptr in ,rest
                     for i from ,(length args)
                     do (setf (cffi:mem-aref ,ptr-var :pointer i) ptr)))
             ,@body))))))


(declaim (inline bool->godot))
(defun bool->godot (value)
  (cond
    ((integerp value) value)
    (value 1)
    (t 0)))


(declaim (inline bool->lisp))
(defun bool->lisp (value)
  (not (zerop value)))


(defun expand-to-godot-bool (value)
  (cond
    ((integerp value) value)
    ((null value) 0)
    ((eq t value) 1)
    (t `(bool->godot ,value))))


(defun expand-from-godot-bool (value)
  `(bool->lisp ,value))


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
               (if (string= "bool" bind)
                   (progn
                     (assert (= size 1))
                    `((cffi:define-foreign-type ,name ()
                        ()
                        (:actual-type :uint8)
                        (:simple-parser ,name))
                      (defmethod cffi:expand-to-foreign (value (type ,name))
                        (declare (ignore type))
                        (expand-to-godot-bool value))
                      (defmethod cffi:expand-from-foreign (value (type ,name))
                        (declare (ignore type))
                        (expand-from-godot-bool value))
                      (defmethod cffi:translate-to-foreign (value (type ,name))
                        (declare (ignore type))
                        (bool->godot value))
                      (defmethod cffi:translate-from-foreign (value (type ,name))
                        (declare (ignore type))
                        (bool->lisp value))))
                   `((cffi:defctype ,name ,(a:switch (bind :test #'equal)
                                             ("Nil" :void)
                                             ("float" (assert (= size 8))
                                                      :double)
                                             ("int" (assert (= size 8))
                                                    :int64)
                                             (t :pointer))))))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(a:switch (bind :test #'equal)
               ("Nil" (list))
               ("float"
                (loop for type in (list :float :double name)
                      collect `(register-godot-method-argument-metadata
                                ',type
                                :alignment ,(cffi:foreign-type-alignment :double)
                                :size ,(cffi:foreign-type-size :double)
                                :translation-expander (lambda (src-val-sym argv-ptr-sym stack-ptr-sym)
                                                        (expand-ptrarg-with-conversion :double
                                                                                       src-val-sym
                                                                                       argv-ptr-sym
                                                                                       stack-ptr-sym)) )))

               ("int"
                (loop for type in (list :uint8 :int8 :uint16 :int16 :uint32 :int32 :int64 :uint64
                                        :int :size 'wchar name)
                      collect `(register-godot-method-argument-metadata
                                ',type
                                :alignment ,(cffi:foreign-type-alignment :int64)
                                :size ,(cffi:foreign-type-size :int64)
                                :translation-expander (lambda (src-val-sym argv-ptr-sym stack-ptr-sym)
                                                        (expand-ptrarg-with-conversion :int64
                                                                                       src-val-sym
                                                                                       argv-ptr-sym
                                                                                       stack-ptr-sym)))))
               ("bool"
                (loop for type in (list :bool name)
                      collect `(register-godot-method-argument-metadata
                                ',type
                                :alignment ,(cffi:foreign-type-alignment :uint8)
                                :size ,(cffi:foreign-type-size :uint8)
                                :translation-expander (lambda (src-val-sym argv-ptr-sym stack-ptr-sym)
                                                        (expand-ptrarg-with-conversion ',name
                                                                                       src-val-sym
                                                                                       argv-ptr-sym
                                                                                       stack-ptr-sym)))))
               (t (if builtin
                      `((register-godot-method-argument-metadata
                         ',name
                         :alignment (cffi:foreign-type-alignment ',name)
                         :size (cffi:foreign-type-size ',name)
                         :translation-expander (lambda (src-val-sym argv-ptr-sym stack-ptr-sym)
                                                 (expand-ptrarg-blob ',name src-val-sym argv-ptr-sym stack-ptr-sym))))

                      `((register-godot-method-argument-metadata
                         ',name
                         :alignment (cffi:foreign-type-alignment ',name)
                         :size (cffi:foreign-type-size ',name)
                         :translation-expander #'expand-ptrarg-pointer)))))
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
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (register-godot-singleton ',name ',class-name))
       (declaim (inline ,name))
       (defun ,name ()
         (get-singleton ,(bind-of extension))))))


(defun extract-variant-value (variant-ptr)
  (let* ((value-type (%gdext:variant-get-type variant-ptr))
         (value-class (get-godot-builtin value-type))
         (value-ptr (%gdext:mem-alloc2 (size-of value-class) 0))
         (ctor (%gdext:get-variant-to-type-constructor value-type)))
    (funcall-prototype ctor %gdext:type-from-variant-constructor-func value-ptr variant-ptr)
    value-ptr))


(defmacro expand-builtin-constant-initializer (class-name name defvar-name bind)
  (let ((class (get-godot-class class-name))
        (variant-class (get-godot-class (uiop:find-symbol* 'variant (symbol-package name)))))
    `(with-godot-string-names ((constant-name ,bind))
       (let ((variant (%gdext:mem-alloc2 ,(size-of variant-class) 0)))
         (unwind-protect
              (progn
                (%gdext:variant-get-constant-value ,(variant-kind-of class) constant-name variant)
                (setf ,defvar-name (extract-variant-value variant)))
           (%gdext:variant-destroy variant)
           (%gdext:mem-free2 variant 0))))))


(defmacro defgconstant (name &key bind value ((:class class-name)) documentation)
  (declare (ignore documentation))
  (let* ((class-name (eval class-name))
         (class (get-godot-class class-name)))
    (if (builtinp class)
        (let ((defvar-name (a:format-symbol :%%godot.util~secret "~A~A" name '-constant-value))
              (initializer-name (a:format-symbol :%%godot.util~secret "~A~A" name '-constant-initializer)))
          `(progn
             (defvar ,defvar-name (cffi:null-pointer))
             (define-symbol-macro ,name (,defvar-name))
             (declaim (inline ,defvar-name))
             (defun ,defvar-name ()
               ,defvar-name)
             (defun ,initializer-name ()
               (the cffi:foreign-pointer
                    (expand-builtin-constant-initializer ,class-name ,name ,defvar-name ,bind)))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (register-godot-builtin-constant ',name :initializer ',initializer-name))))
        `(a:define-constant ,name ,value :test '=))))


(defun bind-godot-constants ()
  (loop for constant-init being the hash-value of *godot-builtin-constant-registry*
        do (funcall constant-init))
  (values))


(defmacro defgenum (name-and-opts &body values)
  (destructuring-bind (name &key class ((:bitfield bitfield-p)))
      (uiop:ensure-list name-and-opts)
    (declare (ignore class))
    `(progn
       (,(if bitfield-p 'cffi:defbitfield 'cffi:defcenum)  ,name
        ,@values)

       (eval-when (:compile-toplevel :load-toplevel :execute)
         (register-godot-method-argument-metadata
          ',name
          :alignment ,(cffi:foreign-type-alignment :int64)
          :size ,(cffi:foreign-type-size :int64)
          :translation-expander (lambda (src-val-sym argv-ptr-sym stack-ptr-sym)
                                  (expand-ptrarg-enum
                                   ',name ,bitfield-p src-val-sym argv-ptr-sym stack-ptr-sym)))))))


(defun expand-vararg-compiler-macro (method-ptr-var bind-extractor args variants)
  `(progn
     (when (cffi:null-pointer-p ,method-ptr-var)
       (setf ,method-ptr-var ,bind-extractor)
       (when (cffi:null-pointer-p ,method-ptr-var)
         (error "Failed to get method pointer with ~A" ',bind-extractor)))
     (call-vararg-method-bind
      ,method-ptr-var
      ,@args
      ,@variants
      nil)))


(defmacro defgmethod (name-and-opts return-type &body arguments)
  (destructuring-bind (lisp-name &key bind ((:class class-name)) hash static vararg virtual)
      (uiop:ensure-list name-and-opts)
    (let* ((class-name (eval class-name))
           (bind (eval bind))
           (extension (get-godot-class class-name))
           (instance-var (a:format-symbol :%%godot.util~secret "~A" 'instance))
           (ret-var (a:format-symbol :%%godot.util~secret "~A" 'result))
           (method-ptr-var (a:format-symbol :%%godot.util~secret "*~A~A*" lisp-name '-method-ptr)))
      (multiple-value-bind (params param-types)
          (loop for (name type) in arguments
                collect name into names
                collect type into types
                finally (return (values names types)))
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
          (a:with-gensyms (variants)
            `(progn
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (register-godot-method ',class-name ',lisp-name
                                        ,hash ,bind
                                        :parameter-types ',param-types
                                        :return-type ',return-type
                                        :vararg ,vararg
                                        :virtual ,virtual))

               (declaim (inline ,lisp-name)
                        (type cffi:foreign-pointer ,method-ptr-var))
               (defvar ,method-ptr-var (cffi:null-pointer))
               ,@(if vararg
                     `((defun ,lisp-name (,@lambda-list &rest ,variants)
                         (when (cffi:null-pointer-p ,method-ptr-var)
                           (setf ,method-ptr-var ,bind-extractor)
                           (when (cffi:null-pointer-p ,method-ptr-var)
                             (error "Failed to get method pointer with ~A" ',bind-extractor)))
                         (call-vararg-method-bind
                          ,method-ptr-var ,(if static
                                               (cffi:null-pointer)
                                               instance-var)
                          ,(unless (eq return-type :void)
                             ret-var)
                          ,@params
                          ,variants))
                       (define-compiler-macro ,lisp-name (,@lambda-list &rest ,variants)
                         (expand-vararg-compiler-macro ',method-ptr-var
                                                       ',bind-extractor
                                                       (list ,(if static
                                                                  (cffi:null-pointer)
                                                                  instance-var)
                                                             ,(unless (eq return-type :void)
                                                                ret-var)
                                                             ,@params)
                                                       ,variants)))
                     `((defun ,lisp-name (,@lambda-list)
                         (when (cffi:null-pointer-p ,method-ptr-var)
                           (setf ,method-ptr-var ,bind-extractor)
                           (when (cffi:null-pointer-p ,method-ptr-var)
                             (error "Failed to get method pointer with ~A" ',bind-extractor)))
                         (,(if (builtinp extension)
                               'call-builtin-method
                               'ptrcall-method-bind)
                          ,class-name ,lisp-name
                          ,method-ptr-var ,(if static
                                               (cffi:null-pointer)
                                               instance-var)
                          ,(unless (eq return-type :void)
                             ret-var)
                          ,@params)))))))))))


(defmacro defgconstructor (name-and-opts &body arguments)
  (destructuring-bind (lisp-name &key ((:class class-name)) index)
      (uiop:ensure-list name-and-opts)
    (let* ((class-name (eval class-name))
           (extension (get-godot-class class-name))
           (index (eval index))
           (instance-var (a:format-symbol :%%godot.util~secret "~A" 'instance)))
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
             (declaim (inline ,lisp-name))
             (defun ,lisp-name (,instance-var ,@params)
               (let ((,ptr-var (get-constructor-bind ,(variant-kind-of extension) ,index)))
                 (call-builtin-constructor ,class-name ,index ,ptr-var ,instance-var ,@params))
               ,instance-var)))))))


(defmacro defgdestructor (name-and-opts)
  (destructuring-bind (lisp-name &key ((:class class-name)))
      (uiop:ensure-list name-and-opts)
    (let* ((class-name (eval class-name))
           (extension (get-godot-class class-name))
           (instance-var (a:format-symbol :%%godot.util~secret "~A" 'instance)))
      (a:with-gensyms (dtor-ptr-var)
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (register-godot-destructor ',class-name))
           (declaim (inline ,lisp-name))
           (defun ,lisp-name (,instance-var)
             (let ((,dtor-ptr-var (%gdext:variant-get-ptr-destructor ,(variant-kind-of extension))))
               (call-builtin-destructor ,dtor-ptr-var ,instance-var)
               ,instance-var)))))))


(defmacro defgproperty (lisp-name class-name &body keys &key index get set)
  (declare (ignore keys))
  (let* ((class-name (eval class-name))
         (get (eval get))
         (set (eval set))
         (index (eval index))
         (instance-var (a:format-symbol :%%godot.util~secret "~A" 'instance))
         (value-var (a:format-symbol :%%godot.util~secret "~A" 'value))
         (result-var (a:format-symbol :%%godot.util~secret "~A" 'result))
         (setter-returns-p (when set
                             (let ((method (get-godot-method class-name set)))
                               (not (eq (return-type-of method) :void))))))
    `(progn
       ,@(when get
           `((declaim (inline ,lisp-name))
             (defun ,lisp-name (,instance-var ,result-var)
               (,get ,instance-var ,result-var ,@(when index (list index)))
               ,result-var)))
       ,@(when set
           `((declaim (inline (setf ,lisp-name)))
             (defun (setf ,lisp-name) (,value-var
                                       ,instance-var
                                       ,@(when setter-returns-p
                                           (list result-var)))
               (,set ,instance-var
                     ,@(when setter-returns-p
                         (list result-var))
                     ,@(when index (list index))
                     ,value-var)
               ,value-var))))))


(defmacro with-godot-alloc ((var size) &body body)
  `(let ((,var (%gdext:mem-alloc2 ,size 0)))
     (unwind-protect
          (progn ,@body)
       (%gdext:mem-free2 ,var 0))))


;;;
;;; GODOT STRING
;;;
(cffi:defcunion godot-string
  (%%data :uint64))

(declaim (inline init-godot-string))
(defun init-godot-string (ptr lisp-string)
  (cffi:with-foreign-string (content-ptr lisp-string :encoding :utf-8)
    (%gdext:string-new-with-utf8-chars ptr content-ptr)))


(defvar *string-destructor-ptr* (cffi:null-pointer))
(declaim (inline destroy-godot-string))
(defun destroy-godot-string (ptr)
  (when (cffi:null-pointer-p *string-destructor-ptr*)
    (setf *string-destructor-ptr* (%gdext:variant-get-ptr-destructor :string)))
  (funcall-prototype *string-destructor-ptr* %gdext:ptr-destructor ptr))


(defmacro with-godot-string ((var lisp-string) &body body)
  `(with-godot-alloc (,var (cffi:foreign-type-size '(:union godot-string)))
     (init-godot-string ,var ,lisp-string)
     (unwind-protect
          (progn ,@body)
       (destroy-godot-string ,var))))


(defmacro with-godot-strings (bindings &body body)
  (if bindings
      (destructuring-bind (var lisp-string) (first bindings)
        `(with-godot-string (,var ,lisp-string)
           (with-godot-strings ,(rest bindings)
             ,@body)))
      `(progn ,@body)))


(declaim (inline godot-string-to-lisp))
(defun godot-string-to-lisp (godot-string)
  (let ((length (%gdext:string-to-utf8-chars godot-string
                                                       (cffi:null-pointer)
                                                       0)))

    (cref:c-with ((foreign-string :char :count (1+ length)))
      (%gdext:string-to-utf8-chars godot-string
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


(declaim (inline init-godot-string-name))
(defun init-godot-string-name (ptr lisp-string)
  (cffi:with-foreign-string (content-ptr lisp-string :encoding :utf-8)
    (%gdext:string-name-new-with-utf8-chars ptr content-ptr)))


(defvar *string-name-destructor-ptr* (cffi:null-pointer))
(declaim (inline destroy-godot-string-name))
(defun destroy-godot-string-name (ptr)
  (when (cffi:null-pointer-p *string-name-destructor-ptr*)
    (setf *string-name-destructor-ptr* (%gdext:variant-get-ptr-destructor :string-name)))
  (funcall-prototype *string-name-destructor-ptr* %gdext:ptr-destructor ptr))


(defmacro with-godot-string-name ((var lisp-string) &body body)
  `(with-godot-alloc (,var (cffi:foreign-type-size '(:union string-name)))
     (init-godot-string-name ,var ,lisp-string)
     (unwind-protect
          (progn ,@body)
       (destroy-godot-string-name ,var))))


(defmacro with-godot-string-names (bindings &body body)
  (if bindings
      (destructuring-bind (var lisp-string) (first bindings)
        `(with-godot-string-name (,var ,lisp-string)
           (with-godot-string-names ,(rest bindings)
             ,@body)))
      `(progn ,@body)))


(defun get-builtin-method (variant-kind method-name hash)
  (with-godot-string-names ((method-string-name method-name))
    (%gdext:variant-get-ptr-builtin-method variant-kind
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
         (funcall-prototype ,method-ptr %gdext:ptr-built-in-method
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
         (funcall-prototype ,ctor-ptr %gdext:ptr-constructor
                            ,instance-ptr
                            ,argv)))))


(defmacro call-builtin-destructor (dtor-ptr instance-ptr)
  `(funcall-prototype ,dtor-ptr %gdext:ptr-destructor ,instance-ptr))


(defun get-method-bind (class-name method-name hash)
  (with-godot-string-names ((class-string-name class-name)
                            (method-string-name method-name))
    (%gdext:classdb-get-method-bind class-string-name
                                    method-string-name
                                    hash)))


(declaim (inline get-constructor-bind))
(defun get-constructor-bind (variant-type index)
  (%gdext:variant-get-ptr-constructor variant-type index))


(defun get-singleton (name)
  (with-godot-string-names ((string-name name))
    (%gdext:global-get-singleton string-name)))


(defmacro ptrcall-method-bind (class-name method-name method-bind object ret
                            &rest args)
  (let* ((method (get-godot-method class-name method-name))
         (param-types (parameter-types-of method)))
    (a:with-gensyms (argv argc)
      `(with-method-argv (,argv ,argc ,@(loop for param-type in param-types
                                              for arg in args
                                              append `(',param-type ,arg)))
         (declare (ignore ,argc))
         (%gdext:object-method-bind-ptrcall
          ,method-bind
          ,object
          ,argv
          ,(if ret ret '(cffi:null-pointer)))
         ,(when ret ret)))))


(defmacro call-vararg-method-bind (method-bind object ret &rest args-and-rest)
  (a:with-gensyms (argv argc)
    `(with-method-varargs (,argv ,argc ,@args-and-rest)
       (%gdext:object-method-bind-call
        ,method-bind
        ,object
        ,argv
        ,argc
        ,(if ret ret '(cffi:null-pointer))
        (cffi:null-pointer))
       ,(when ret ret))))



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


(register-godot-method-argument-metadata
 :pointer
 :alignment (cffi:foreign-type-alignment :pointer)
 :size (cffi:foreign-type-size :pointer)
 :translation-expander #'expand-ptrarg-pointer)
