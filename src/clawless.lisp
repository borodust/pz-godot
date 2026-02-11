(cl:in-package :pz-godot)

(declaim (special *exports*))

(defparameter *extension-api-file* (asdf:system-relative-pathname :pz-godot/wrapper "src/api/extension_api.json"))

(defparameter *gdextension-interface-file* (asdf:system-relative-pathname :pz-godot/wrapper "src/api/gdextension_interface.json"))

(defparameter *gdextension-type-bindings-file* (asdf:system-relative-pathname :pz-godot/wrapper "bindings/gdext-types.lisp"))

(defparameter *gdextension-interface-bindings-file* (asdf:system-relative-pathname :pz-godot/wrapper "bindings/gdext-interface.lisp"))

(defparameter *type-string-regex* (ppcre:create-scanner "(const)?\\s*(\\w+)\\s*(\\W+)?"))

(defparameter *camel-case-splitter* (ppcre:create-scanner "([A-Z]+[^A-Z]*)+?"))

(defparameter *constant-splitter* (ppcre:create-scanner "([A-Z]+[^A-Z]*)+?"))



(defun keywordify (name)
  (a:make-keyword (uiop:standard-case-symbol-name name)))


(defun symbolicate-gdext-camel-case (name &key (package *package*) (skip-first t))
  (let ((parts (ppcre:all-matches-as-strings *camel-case-splitter* name)))
    (a:format-symbol package "~{~A~^-~}"
                     (mapcar #'uiop:standard-case-symbol-name
                             (if skip-first
                                 (rest parts)
                                 parts)))))


(defun symbolicate-gdext-snake-case (name &key (package *package*) (skip-first t))
  (let ((parts (ppcre:split "_" name)))
    (a:format-symbol package "~{~A~^-~}"
                     (mapcar #'uiop:standard-case-symbol-name
                             (if skip-first
                                 (rest parts)
                                 parts)))))


(defun parse-type-string (type-string &key (package *package*))
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings *type-string-regex* type-string)
    (declare (ignore match))
    (let ((type (let ((type-name (aref groups 1)))
                  (if (a:starts-with-subseq "GDExtension" type-name)
                      (symbolicate-gdext-camel-case type-name :package package)
                      (a:eswitch (type-name :test #'equal)
                        ("int8_t" :int8)
                        ("uint8_t" :uint8)
                        ("int16_t" :int16)
                        ("uint16_t" :uint16)
                        ("int32_t" :int32)
                        ("uint32_t" :uint32)
                        ("int64_t" :int64)
                        ("uint64_t" :uint64)
                        ("float" :float)
                        ("double" :double)
                        ("char" :char)
                        ("char16_t" :uint16)
                        ("char32_t" :uint32)
                        ("size_t" :size)
                        ("void" :void)

                        ("wchar_t" '%gdext.common:wchar)
                        ("GDObjectInstanceID"
                         (symbolicate-gdext-camel-case type-name :package package)))))))
      (a:eswitch ((aref groups 2) :test #'equal)
        (nil type)
        ("*" `(:pointer ,type))
        ("**" `(:pointer (:pointer ,type)))))))


(defun expand-gdext-documentation (description deprecated)
  (when (or description deprecated)
    (let ((deprecation-text (when deprecated
                              (let ((since (gethash "since" deprecated)))
                                (a:if-let ((replaced-with (gethash "replaced_with" deprecated)))
                                  (format nil "REPLACED with ~A since ~A" replaced-with since)
                                  (format nil "DEPRECATED since ~A" since))))))
      (list (format nil "~@[~A~]~@[~{~&~A~}~]" deprecation-text (coerce description 'list))))))


(defgeneric explode-gdext-type (out kind &key &allow-other-keys))

(defmethod explode-gdext-type :before (out kind &key)
  (format out "~&~%"))


(defmethod explode-gdext-type (out (kind (eql :alias)) &key name
                                                         description
                                                         type
                                                         deprecated)
  (let ((namesym (symbolicate-gdext-camel-case name)))
    (push namesym *exports*)
    (prin1
     `(cffi:defctype ,(symbolicate-gdext-camel-case name)
          ,(parse-type-string type)
        ,@(expand-gdext-documentation description deprecated))
     out)))


(defmethod explode-gdext-type (out (kind (eql :handle)) &key name
                                                          description
                                                          parent
                                                          ((:is_const const-p))
                                                          ((:is_uninitialized uninitialized-p))
                                                          deprecated)
  (declare (ignore const-p uninitialized-p parent))
  (let ((namesym (symbolicate-gdext-camel-case name)))
    (push namesym *exports*)
    (prin1
     `(cffi:defctype ,namesym
          (:pointer :void)
        ,@(expand-gdext-documentation description deprecated))
     out)))

(defmethod explode-gdext-type (out (kind (eql :enum)) &key name
                                                        description
                                                        ((:is_bitfield bitfield-p))
                                                        values
                                                        deprecated)
  (let ((namesym (symbolicate-gdext-camel-case name))
        (common-prefix-idx
          (if (<= (length values) 1)
              0
              (loop with first = (gethash "name" (aref values 0))
                    for value across values
                    for other = (gethash "name" value)
                    unless (string= first other)
                      minimize (mismatch first other)))))
    (push namesym *exports*)
    (prin1
     `(,(if bitfield-p 'cffi:defbitfield 'cffi:defcenum)
       ,namesym
       ,@(expand-gdext-documentation description deprecated)
       ,@(loop for value across values
               for name = (subseq (gethash "name" value) common-prefix-idx)
               collect `(,(keywordify (symbolicate-gdext-snake-case name
                                                                    :skip-first nil))
                         ,(gethash "value" value))))
     out)))


(defmethod explode-gdext-type (out (kind (eql :function)) &key name
                                                            description
                                                            arguments
                                                            ((:return_value return-type))
                                                            deprecated)
  (let ((namesym (symbolicate-gdext-camel-case name)))
    (push namesym *exports*)
    (prin1
     `(cffi:defctype ,namesym (:pointer :void)
        ,@(expand-gdext-documentation description deprecated))
     out)
    (format out "~&~%")
    (let ((cffi-return-type (if return-type
                                (parse-type-string (gethash "type" return-type))
                                :void))
          (cffi-param-types (when arguments
                              (loop for arg-def across arguments
                                    collect (parse-type-string (gethash "type" arg-def))))))
      (prin1
       `(%gdext.common:defcfunproto ,namesym ,cffi-return-type
          ,@cffi-param-types)
       out))))


(defmethod explode-gdext-type (out (kind (eql :struct)) &key name
                                                          description
                                                          members
                                                          deprecated)
  (let ((namesym (symbolicate-gdext-camel-case name)))
    (push namesym *exports*)
    (prin1
     `(cffi:defcstruct ,namesym
        ,@(expand-gdext-documentation description deprecated)
        ,@(loop for member across members
                collect `(,(symbolicate-gdext-snake-case (gethash "name" member)
                                                         :skip-first nil)
                          ,(parse-type-string (gethash "type" member)))))

     out)
    (terpri out)
    (prin1 `(cffi:defctype ,namesym (:struct ,namesym)) out)))


(defun explode-gdext-types (out types)
  (loop for type across types
        do (multiple-value-bind (kind rest-def)
               (loop with kind = nil
                     for key being the hash-key in type
                       using (hash-value value)
                     for keysym = (keywordify key)
                     if (eq :kind keysym)
                       do (setf kind (keywordify value))
                     else
                       append (list keysym value) into rest-def
                     finally (return (values kind rest-def)))
             (apply #'explode-gdext-type out kind rest-def))))


(defun generate-gdext-type-bindings (types)
  (a:with-output-to-file (out *gdextension-type-bindings-file*
                              :if-exists :supersede)
    (let ((*print-case* :downcase)
          (*print-pretty* t)
          (*package* (find-package :pz-godot-pristine))
          (*exports*))
      (prin1 '(uiop:define-package :%gdext.types (:use)) out)
      (terpri out)
      (prin1 '(cl:in-package :%gdext.types) out)
      (explode-gdext-types out types)
      (terpri out)
      (terpri out)
      (prin1 `(cl:export '(,@(nreverse *exports*))) out))))


(defun explode-gdext-interface-function (out function-def)
  (let* ((function-name (gethash "name" function-def))
         (namesym (symbolicate-gdext-snake-case function-name
                                                :skip-first nil))
         (arguments (loop for arg-def across (gethash "arguments" function-def)
                          for name = (symbolicate-gdext-snake-case
                                      (gethash "name" arg-def)
                                      :skip-first nil)
                          for type = (parse-type-string
                                      (gethash "type" arg-def)
                                      :package (find-package :%gdext.types))
                          collect (list name type))))
    (push namesym *exports*)
    (prin1
     `(%gdext.common:defifun
          (,function-name ,namesym)
          ,(a:if-let ((return-type (gethash "return_value" function-def)))
             (parse-type-string (gethash "type" return-type)
                                :package (find-package :%gdext.types))
             :void)
        ,@arguments)
     out)))


(defun explode-gdext-interface (out interface)
  (loop for function across interface
        do (format out "~&~%")
           (explode-gdext-interface-function out function)))


(defun generate-gdext-interface-bindings (interface)
  (uiop:ensure-package :%gdext.types)
  (a:with-output-to-file (out *gdextension-interface-bindings-file*
                              :if-exists :supersede)
    (let ((*print-case* :downcase)
          (*print-pretty* t)
          (*package* (find-package :cl))
          (*exports*))
      (prin1 '(uiop:define-package :%gdext.interface (:use :cl)) out)
      (terpri out)
      (let ((*package* (find-package :pz-godot-pristine)))
        (prin1 '(cl:in-package :%gdext.interface) out))
      (explode-gdext-interface out interface)
      (terpri out)
      (terpri out)
      (prin1 `(cl:export '(,@(nreverse *exports*))) out))))


(defun generate-gdext-bindings ()
  (let* ((root (jzon:parse *gdextension-interface-file*))
         (format-version (gethash "format_version" root)))
    (unless (= format-version 1)
      (error "Unsupported version format: ~A" format-version))
    (generate-gdext-type-bindings (gethash "types" root))
    (generate-gdext-interface-bindings (gethash "interface" root))))


(defun regenerate-bindings ()
  (generate-gdext-bindings)
  (values))
