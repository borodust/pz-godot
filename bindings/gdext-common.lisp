(cl:defpackage :%gdext.util
  (:use :cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:cref #:cffi-c-ref))
  (:export #:wchar
           #:memcpy

           #:defcfunproto
           #:defprotocallback
           #:get-protocallback
           #:funcall-prototype

           #:defifun
           #:bind-interface

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
           #:godot-extension-method-bind-name))
(cl:defpackage :%%gdext.util~secret
  (:use))
(cl:in-package :%gdext.util)


(defvar *function-prototype-registry* (make-hash-table))

(defvar *interface-registry* (make-hash-table))

(defvar *godot-method-argument-metadata* (make-hash-table :test 'eq))

(cffi:defctype wchar #+windows :uint16 #-windows :uint32)

(defun format-symbol-into (package control-string &rest args)
  (uiop:intern* (apply #'format nil control-string args) package))


(cffi:defcfun ("memcpy" memcpy) :pointer
  (dst :pointer)
  (src :pointer)
  (size :size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FUNCTION PROTO
;;
(defun register-function-prototype (name result-type &rest parameter-types)
  (setf (gethash name *function-prototype-registry*) (list* result-type parameter-types)))


(defmacro defcfunproto (name result-type &body parameter-types)
  `(register-function-prototype ',name ',result-type ,@(loop for type in parameter-types
                                                             collect `(quote ,type) )))


(defmacro funcall-prototype (ptr name &rest args)
  (let* ((proto (gethash name *function-prototype-registry*)))
    (unless proto
      (error "Function prototype for name ~A not found" name))
    (unless (= (length (rest proto)) (length args))
      (error "Unexpected number of arguments: expected ~A, got ~A"
             (length (rest proto))
             (length args)))
    (let ((type-args (reduce #'append (mapcar #'list (rest proto) args))))
      `(cffi:foreign-funcall-pointer ,ptr ()
                                     ,@type-args
                                     ,(first proto)))))


#+sbcl
(progn
  (defun get-protocallback (callback-name)
    (sb-alien:alien-sap (sb-alien:alien-callable-function callback-name)))
  (defun expand-defcallback (callback-name return-type arg-names-and-types body)
    (multiple-value-bind (body declarations)
        (a:parse-body body :documentation t)
      `(sb-alien:define-alien-callable ,callback-name
           ,(cffi-sys::convert-foreign-type
             (cffi::canonicalize-foreign-type return-type))
           (,@(loop for (name type) in arg-names-and-types
                    collect (list name (cffi-sys::convert-foreign-type
                                        (cffi::canonicalize-foreign-type type)))))
         ,(multiple-value-bind (arg-names arg-types)
              (loop for (name type) in arg-names-and-types
                    collect name into arg-names
                    collect type into arg-types
                    finally (return (values arg-names arg-types)))
            (cffi::inverse-translate-objects
             arg-names arg-types declarations return-type
             `(block ,callback-name ,@body)))))))

#-(or sbcl)
(progn
  (defun get-protocallback (callback-name)
    (cffi:get-callback callback-name))
  (defun expand-defcallback (callback-name return-type arg-types body)
    `(cffi:defcallback ,callback-name ,return-type (,@arg-types)
       ,@body)))


(defmacro defprotocallback (name (&rest args) &body body)
  (destructuring-bind (callback-name &optional prototype-name)
      (if (listp name) name (list name))
    (let* ((prototype-name (or prototype-name callback-name))
           (proto (gethash prototype-name *function-prototype-registry*)))
      (unless proto
        (error "Function prototype for name ~A not found" prototype-name))
      (unless (= (length (rest proto)) (length args))
        (error "Unexpected number of arguments: expected ~A, got ~A"
               (length (rest proto))
               (length args)))
      (let ((arg-types (mapcar #'list args (rest proto))))
        (expand-defcallback callback-name (first proto) arg-types body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INTERFACE FUNC
;;
(defun register-interface-function (lisp-name c-name ptr-var-name)
  (setf (gethash lisp-name *interface-registry*) (cons c-name ptr-var-name)))


(defmacro defifun ((c-name lisp-name) return-type
                   &body parameters)
  (let ((param-names (mapcar #'first parameters))
        (ptr-var-name (format-symbol-into '%%gdext.util~secret "*~A~~~A*" 'function-pointer lisp-name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defcfunproto ,lisp-name ,return-type ,@(mapcar #'second parameters)))
       (defvar ,ptr-var-name (cffi:null-pointer))
       (declaim (inline ,lisp-name))
       (defun ,lisp-name (,@param-names)
         (funcall-prototype ,ptr-var-name ,lisp-name ,@param-names))
       (register-interface-function
        ',lisp-name ,c-name ',ptr-var-name))))


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
         (cffi:mem-ref ,stack-ptr-sym ',arg-type-to) ,(a:eswitch (arg-type-to)
                                                        (:int64 `(truncate ,src-val-sym))
                                                        (:double `(float ,src-val-sym 0d0))
                                                        (:uint8 `(truncate ,src-val-sym)))))

(defun expand-ptrarg-blob (arg-type src-val-sym argv-ptr-sym stack-ptr-sym)
  `(progn
     (setf (cffi:mem-ref ,argv-ptr-sym :pointer) ,stack-ptr-sym)
     (memcpy ,stack-ptr-sym ,src-val-sym (cffi:foreign-type-size ',arg-type))))


(defun expand-ptrarg-pointer (src-val-sym argv-ptr-sym stack-ptr-sym)
  `(setf (cffi:mem-ref ,argv-ptr-sym :pointer) ,stack-ptr-sym
         (cffi:mem-ref ,stack-ptr-sym :pointer) ,src-val-sym))
