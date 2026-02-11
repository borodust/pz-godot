(cl:defpackage :%gdext.common
  (:use :cl)
  (:export #:wchar
           #:defcfunproto
           #:defprotocallback
           #:funcall-prototype

           #:defifun
           #:initialize-interface))
(cl:defpackage :%%gdext.common~secret
  (:use))
(cl:in-package :%gdext.common)


(defvar *function-prototype-registry* (make-hash-table))

(defvar *interface-registry* (make-hash-table))

(cffi:defctype wchar #+windows :uint16 #-windows :uint32)

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
  (let* ((name (eval name))
         (proto (gethash name *function-prototype-registry*)))
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


(defmacro defprotocallback (name (&rest args) &body body)
  (destructuring-bind (callback-name &optional prototype-name) (if (listp name) name (list name))
    (let* ((prototype-name (or prototype-name callback-name))
           (proto (gethash prototype-name *function-prototype-registry*)))
      (unless proto
        (error "Function prototype for name ~A not found" prototype-name))
      (unless (= (length (rest proto)) (length args))
        (error "Unexpected number of arguments: expected ~A, got ~A"
               (length (rest proto))
               (length args)))
      (let ((arg-types (mapcar #'list args (rest proto))))
        `(cffi:defcallback ,callback-name ,(first proto) (,@arg-types)
           ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INTERFACE FUNC
;;
(defun register-interface-function (lisp-name c-name initializer)
  (setf (gethash lisp-name *interface-registry*) (cons c-name initializer)))


(defmacro defifun ((c-name lisp-name) return-type
                   &body parameters)
  (let ((param-names (mapcar #'first parameters)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defcfunproto ,lisp-name ,return-type ,@(mapcar #'second parameters)))
       (let ((%%gdext.common~secret::ptr (cffi:null-pointer)))
         (declare (type cffi:foreign-pointer %%gdext.common~secret::ptr)
                  (optimize (safety 0)))
         (defun ,lisp-name (,@param-names)
           (funcall-prototype %%gdext.common~secret::ptr ',lisp-name ,@param-names))
         (register-interface-function
          ',lisp-name ,c-name
          (lambda (ptr)
            (declare (optimize (safety 0))
                     (type cffi:foreign-pointer ptr))
            (setf %%gdext.common~secret::ptr ptr)))))))


(defun initialize-interface (get-proc-address-ptr)
  (flet ((%get-proc-address (c-name)
           (cffi:foreign-funcall-pointer get-proc-address-ptr ()
                                         :string c-name
                                         :pointer)))
    (loop for function-name being the hash-key in *interface-registry*
            using (hash-value (c-name . initializer))
          do (let ((function-ptr (%get-proc-address c-name)))
               (declare (type (function (cffi:foreign-pointer)
                                        cffi:foreign-pointer)
                              initializer))
               (funcall initializer function-ptr))))
  (values))
