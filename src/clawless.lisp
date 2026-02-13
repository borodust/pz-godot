(cl:in-package :pz-godot)

(declaim (special *exports*
                  *builtin-size-table*
                  *builtin-field-table*
                  *class-method-table*
                  *class-constructor-table*
                  *class-destructor-table*
                  *class-properties-table*
                  *precision*))

(defparameter *gdextension-interface-file* (asdf:system-relative-pathname :pz-godot/wrapper "src/api/gdextension_interface.json"))

(defparameter *gdextension-type-bindings-file* (asdf:system-relative-pathname :pz-godot/wrapper "bindings/gdext-types.lisp"))

(defparameter *gdextension-interface-bindings-file* (asdf:system-relative-pathname :pz-godot/wrapper "bindings/gdext-interface.lisp"))

(defparameter *extension-api-file* (asdf:system-relative-pathname :pz-godot/wrapper "src/api/extension_api.json"))

(defparameter *extension-api-bindings-file* (asdf:system-relative-pathname :pz-godot/wrapper "bindings/godot-extensions.lisp"))

(defparameter *type-string-regex* (ppcre:create-scanner "(const)?\\s*(\\w+)\\s*(\\W+)?"))

(defparameter *extension-type-string-regex* (ppcre:create-scanner "(\\w+::)?(\\d+\/\\d+:)?(\\w+\\.)?(.+)"))

(defparameter *pascal-case-splitter* (ppcre:create-scanner "([A-Z0-9]+[^A-Z0-9]*)+?"))


(defun keywordify (name)
  (a:make-keyword (uiop:standard-case-symbol-name name)))


(defun symbolicate-gdext-pascal-case (name &key (package *package*)
                                            (skip-first t)
                                            prefix
                                            postfix)
  (flet ((%symbolicate-name (name)
           (let ((parts (ppcre:all-matches-as-strings *pascal-case-splitter* name)))
             (a:format-symbol package "~@[~A~]~{~A~^-~}~@[~A~]"
                              prefix
                              (mapcar #'uiop:standard-case-symbol-name
                                      (if skip-first
                                          (rest parts)
                                          parts))
                              postfix))))
    (a:format-symbol package "~{~A~^+~}"
                     (mapcar #'%symbolicate-name
                             (ppcre:split "\\." (string-capitalize name
                                                                   :start 0
                                                                   :end 1))))))


(defun symbolicate-gdext-snake-case (name &key
                                            (package *package*)
                                            (skip-first t)
                                            prefix
                                            postfix)
  (let ((parts (ppcre:split "_" name)))
    (a:format-symbol package "~@[~A~]~{~A~^-~}~@[~A~]"
                     prefix
                     (mapcar #'uiop:standard-case-symbol-name
                             (if skip-first
                                 (rest parts)
                                 parts))
                     postfix)))


(defun parse-type-string (type-string &key (package *package*)
                                        (skip-first t))
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings *type-string-regex* type-string)
    (declare (ignore match))
    (let ((type (let ((type-name (aref groups 1)))
                  (cond
                    ((string= "GDObjectInstanceID" type-name)
                     (symbolicate-gdext-pascal-case type-name :package package))
                    ((upper-case-p (aref type-name 0))
                     (symbolicate-gdext-pascal-case type-name
                                                    :package package
                                                    :skip-first skip-first))
                    (t (a:eswitch (type-name :test #'equal)
                         ("int8_t" :int8)
                         ("uint8_t" :uint8)
                         ("int8" :int8)
                         ("uint8" :uint8)

                         ("int16_t" :int16)
                         ("uint16_t" :uint16)
                         ("int16" :int16)
                         ("uint16" :uint16)

                         ("int32_t" :int32)
                         ("uint32_t" :uint32)
                         ("int32" :int32)
                         ("uint32" :uint32)

                         ("int64_t" :int64)
                         ("uint64_t" :uint64)
                         ("int64" :int64)
                         ("uint64" :uint64)

                         ("float" :float)
                         ("double" :double)

                         ("char" :char)
                         ("char16_t" :uint16)
                         ("char32_t" :uint32)

                         ("bool" :bool)
                         ("int" :int)
                         ("size_t" :size)
                         ("size" :size)
                         ("void" :void)

                         ("wchar_t" '%gdext.util:wchar)
                         ("real_t" (ecase *precision*
                                     (:single :float)
                                     (:double :double)))))))))
      (a:eswitch ((aref groups 2) :test #'equal)
        (nil type)
        ("*" `(:pointer ,type))
        ("**" `(:pointer (:pointer ,type)))))))


(defun common-prefix-idx (values)
  (when (<= (length values) 1)
    (return-from common-prefix-idx 0))
  (let ((values (mapcar #'string values)))
    (let ((mismatch-idx (loop with first = (first values)
                              for other in values
                              unless (string= first other)
                                minimize (mismatch first other))))
      ;; sometimes unprefixed name can start with the same letter, e.g. Clockwise and Counterclockwise
      ;; without looking back the first C will be cut out leaving lockwise and ounterclockwise
      ;; as unprefixed versions, so we track back slightly here
      (flet ((%split-char-p (char)
               (char= #\- char)))
        (a:if-let ((pos (position-if #'%split-char-p (first values) :from-end t :end mismatch-idx)))
          (1+ pos)
          0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GDExtension API
;;
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
  (let ((namesym (symbolicate-gdext-pascal-case name)))
    (push namesym *exports*)
    (prin1
     `(cffi:defctype ,(symbolicate-gdext-pascal-case name)
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
  (let ((namesym (symbolicate-gdext-pascal-case name)))
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
  (let ((namesym (symbolicate-gdext-pascal-case name))
        (common-prefix-idx (common-prefix-idx (loop for def across values
                                                    collect (symbolicate-gdext-snake-case
                                                             (gethash "name" def)
                                                             :skip-first nil)))))
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
  (let ((namesym (symbolicate-gdext-pascal-case name)))
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
       `(%gdext.util:defcfunproto ,namesym ,cffi-return-type
          ,@cffi-param-types)
       out))))


(defmethod explode-gdext-type (out (kind (eql :struct)) &key name
                                                          description
                                                          members
                                                          deprecated)
  (let ((namesym (symbolicate-gdext-pascal-case name)))
    (push namesym *exports*)
    (prin1
     `(cffi:defcstruct ,namesym
        ,@(expand-gdext-documentation description deprecated)
        ,@(loop for member across members
                for field-name = (symbolicate-gdext-snake-case (gethash "name" member)
                                                               :skip-first nil)
                do (push field-name *exports*)
                collect `(,field-name
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
     `(%gdext.util:defifun
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
    (uiop:ensure-package :%gdext.types)
    (generate-gdext-type-bindings (gethash "types" root))
    (generate-gdext-interface-bindings (gethash "interface" root))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Godot Extensions API
;;
(defun parse-extension-type-string (type-string)
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings *extension-type-string-regex* type-string)
    (declare (ignore match))
    (let ((kind (a:switch ((aref groups 0) :test #'equal)
                  ("enum::" :enum)
                  ("bitfield::" :bitfield)
                  ("typedarray::" :typed-array)
                  ("typeddictionary::" :typed-dictionary)
                  (nil :other)
                  (t :unknown)))
          (hint (aref groups 1))
          (owner (aref groups 2))
          (type (aref groups 3)))
      (case kind
        (:typed-array (parse-extension-type-string "Array"))
        (:typed-dictionary (parse-extension-type-string "Dictionary"))
        (t (a:switch (type :test #'string=)
             ("bool" (a:format-symbol *package* "~A" 'bool))
             ("float" (a:format-symbol *package* "~A" 'float))
             ("int" (a:format-symbol *package* "~A" 'int))
             (t (let ((parsed-type (if (search "GDExtension" type)
                                       (parse-type-string type :package (find-package :%gdext.types))
                                       (parse-type-string type :skip-first nil))))
                  (if (and (symbolp parsed-type)
                           (not (keywordp parsed-type)))
                      (a:format-symbol *package* "~@[~A+~]~A"
                                       (when owner
                                         (symbolicate-gdext-pascal-case owner :skip-first nil))
                                       parsed-type)
                      parsed-type)))))))))

(defun symbolicate-method-name (method-def class-name)
  (let* ((name (gethash "name" method-def)))
    (a:format-symbol *package*
                     "~A+~A"
                     class-name
                     (symbolicate-gdext-snake-case name
                                                   :skip-first nil))))

(defun explode-extension-method (out class-name method-def)
  (let* ((name (gethash "name" method-def))
         (vararg-p (gethash "is_vararg" method-def))
         (static-p (gethash "is_static" method-def))
         (virtual-p (gethash "is_virtual" method-def))
         (hash (gethash "hash" method-def))
         (return-type (a:if-let ((ret-type (gethash "return_type" method-def)))
                        ret-type
                        (a:when-let ((ret-val (gethash "return_value" method-def)))
                          (gethash "type" ret-val))))
         (parameters (gethash "arguments" method-def))
         (namesym (symbolicate-method-name method-def class-name)))
    (push namesym *exports*)
    (format out "~&~%")
    (prin1
     `(%gdext.util:defgmethod (,namesym
                               :class ',class-name
                               :bind ,name
                               :hash ,hash
                               ,@(when static-p
                                   '(:static t))
                               ,@(when vararg-p
                                   '(:vararg t))
                               ,@(when virtual-p
                                   '(:virtual t)))
          ,(if return-type
               (parse-extension-type-string return-type)
               :void)
        ,@(when parameters
            (loop for param-def across parameters
                  for param-name = (gethash "name" param-def)
                  for param-type = (gethash "type" param-def)
                  collect `(,(symbolicate-gdext-snake-case param-name
                                                           :skip-first nil)
                            ,(parse-extension-type-string param-type)))))
     out)))


(defun explode-extension-constructor (out class-name constructor-def)
  (let* ((idx (gethash "index" constructor-def))
         (parameters (gethash "arguments" constructor-def))
         (namesym (a:format-symbol *package*
                                   "~A-~A~@[@~A~]"
                                   'make
                                   class-name
                                   (when (> idx 0) idx))))
    (push namesym *exports*)
    (format out "~&~%")
    (prin1
     `(%gdext.util:defgconstructor (,namesym
                                    :class ',class-name
                                    :index ,idx)
        ,@(when parameters
            (loop for param-def across parameters
                  for param-name = (gethash "name" param-def)
                  for param-type = (gethash "type" param-def)
                  collect `(,(symbolicate-gdext-snake-case param-name
                                                           :skip-first nil)
                            ,(parse-extension-type-string param-type)))))
     out)))


(defun explode-extension-destructor (out class-name)
  (let* ((namesym (a:format-symbol *package*
                                   "~A~A"
                                   'destroy-
                                   class-name)))
    (push namesym *exports*)
    (format out "~&~%")
    (prin1
     `(%gdext.util:defgdestructor (,namesym :class ',class-name))
     out)))


(defun explode-extension-property (out class-name property-def)
  (flet ((%find-accessor (name)
           (a:when-let ((methods (gethash class-name *class-method-table*)))
             (loop for method across methods
                   when (string= name (gethash "name" method))
                     do (return (symbolicate-method-name method class-name))))))
    (let* ((prop-name (gethash "name" property-def))
           (getter (%find-accessor (gethash "getter" property-def)))
           (setter (%find-accessor (gethash "setter" property-def)))
           (index (gethash "index" property-def))
           (namesym (a:format-symbol *package*
                                     "~A+~A"
                                     class-name
                                     (symbolicate-gdext-snake-case prop-name :skip-first nil))))
      (push namesym *exports*)
      (format out "~&~%")
      (prin1
       `(%gdext.util:defgproperty ,namesym ',class-name
          ,@(when index `(:index ,index))
          ,@(when getter `(:get ',getter))
          ,@(when setter `(:set ',setter)))
       out))))


(defun explode-extension-class (out class-def &key ((:builtin builtin-p)))
  (let* ((name (gethash "name" class-def))
         (refcounted-p (gethash "is_refcounted" class-def))
         (instantiable-p (gethash "is_instantiable" class-def))
         (inherits (gethash "inherits" class-def))
         (api-type (gethash "api_type" class-def))
         (constants (gethash "constants" class-def))
         (enums (gethash "enums" class-def))
         (methods (gethash "methods" class-def))
         (constructors (gethash "constructors" class-def))
         (has-destructor-p (gethash "has_destructor" class-def))
         (signals (gethash "signals" class-def))
         (properties (gethash "properties" class-def))
         (namesym (symbolicate-gdext-pascal-case name
                                                 :skip-first nil)))
    (push namesym *exports*)
    (format out "~&~%")
    (prin1
     `(%gdext.util:defgclass (,namesym
                              :bind ,name
                              :api ,(if builtin-p
                                        :builtin
                                        (a:eswitch (api-type :test #'string=)
                                          ("core" :core)
                                          ("editor" :editor)))
                              ,@(when builtin-p
                                  `(:size ,(gethash name *builtin-size-table*))))
        ,@(a:when-let ((fields (gethash name *builtin-field-table*)))
            `((:fields ,@(loop for (field-name field-offset field-type) in fields
                               collect (list (symbolicate-gdext-snake-case field-name :skip-first nil)
                                             (parse-extension-type-string field-type)
                                             :offset field-offset)))))
        ,@(when signals
            `((:signals ,@(loop for signal-def across signals
                                for signal-name = (gethash "name" signal-def)
                                for signal-params = (gethash "arguments" signal-def)
                                collect (list* (symbolicate-gdext-snake-case signal-name :skip-first nil)
                                               (when signal-params
                                                 (loop for param-def across signal-params
                                                       for param-name = (gethash "name" param-def)
                                                       for param-type = (gethash "type" param-def)
                                                       append (list (symbolicate-gdext-snake-case param-name
                                                                                                  :skip-first nil)
                                                                    (parse-extension-type-string param-type))))))))))
     out)
    (when enums
      (loop for enum-def across enums
            do (explode-extension-enum out enum-def
                                       :class namesym
                                       :prefix (a:symbolicate namesym '+))))
    (when methods
      (setf (gethash namesym *class-method-table*) methods))
    (when constructors
      (setf (gethash namesym *class-constructor-table*) constructors))
    (when has-destructor-p
      (setf (gethash namesym *class-destructor-table*) namesym))
    (when properties
      (setf (gethash namesym *class-properties-table*) properties))))


(defun explode-native-structures (out struct-def)
  (flet ((%parse-field (field-def)
           (let* ((field-def-no-default (first (ppcre:split "\\s*=\\s*" field-def)))
                  (index-def (first (ppcre:all-matches-as-strings "\\s*(\\[\\d+\\])+\\s*$" field-def-no-default)))
                  (field-array-size (when index-def
                                      (reduce #'*
                                              (mapcar #'parse-integer
                                                      (remove-if #'a:emptyp
                                                                 (ppcre:split "[\\s\\[\\]]" index-def))))))
                  (type-and-name (if index-def
                                     (subseq field-def-no-default 0 (- (length field-def-no-default)
                                                                       (length index-def)))
                                     field-def-no-default)))
             (multiple-value-bind (name-match name-groups)
                 (ppcre:scan-to-strings "\\s*(\\w+)\\s*$" type-and-name)
               (let* ((field-name (aref name-groups 0))
                      (field-type (subseq type-and-name 0 (- (length type-and-name)
                                                             (length name-match)))))
                 (values (symbolicate-gdext-snake-case field-name :skip-first nil)
                         (let ((parsed-type (parse-extension-type-string
                                             (ppcre:regex-replace-all "::" field-type "."))))
                           (if field-array-size
                               `(:array ,parsed-type ,field-array-size)
                               parsed-type))))))))
    (let* ((name (gethash "name" struct-def))
           (encoded-fields (gethash "format" struct-def))
           (namesym (symbolicate-gdext-pascal-case name :skip-first nil)))
      (push namesym *exports*)
      (format out "~&~%")
      (prin1 `(cffi:defcstruct ,namesym
                ,@(loop for field-def in (ppcre:split "\\s*;\\s*" encoded-fields)
                        collect (multiple-value-bind (field-name field-type)
                                    (%parse-field field-def)
                                  (list field-name field-type))))
             out)
      (prin1 `(cffi:defctype ,namesym (:struct ,namesym)) out))))


(defun explode-singleton (out singleton-def)
  (let* ((name (gethash "name" singleton-def))
         (type (gethash "type" singleton-def))
         (namesym (symbolicate-gdext-pascal-case name :skip-first nil))
         (typesym (symbolicate-gdext-pascal-case type :skip-first nil)))
    (push namesym *exports*)
    (format out "~&~%")
    (prin1
     `(%gdext.util:defgsingleton ,namesym ',typesym)
     out)))


(defun explode-extension-enum (out enum-def &key prefix ((:class class-name)))
  (let* ((name (gethash "name" enum-def))
         (values (gethash "values" enum-def))
         (bitfield-p (gethash "is_bitfield" enum-def))
         (namesym (symbolicate-gdext-pascal-case name
                                                 :skip-first nil
                                                 :prefix prefix))
         (common-prefix-idx (common-prefix-idx (loop for def across values
                                                     collect (symbolicate-gdext-snake-case
                                                              (gethash "name" def)
                                                              :skip-first nil)))))
    (push namesym *exports*)
    (format out "~&~%")
    (pprint
     (let ((name-and-opts `(,namesym
                            ,@(when bitfield-p '(:bitfield t))
                            ,@(when class-name `(:class ',class-name)))))
       `(%gdext.util:defgenum ,(if (rest name-and-opts)
                                   name-and-opts
                                   (first name-and-opts))
          ,@(loop for value across values
                  for name = (subseq (gethash "name" value) common-prefix-idx)
                  collect `(,(keywordify (symbolicate-gdext-snake-case name
                                                                       :skip-first nil))
                            ,(gethash "value" value)))))
     out)))


(defun generate-godot-extension-bindings ()
  (let* ((root (jzon:parse *extension-api-file*))
         (header (gethash "header" root))
         (*precision* (a:eswitch ((gethash "precision" header) :test #'equal)
                        ("single" :single)
                        ("double" :double)))
         (builtin-class-sizes (loop for build-config
                                      across (gethash "builtin_class_sizes" root)
                                    when (string= "float_64"
                                                  (gethash "build_configuration" build-config))
                                      do (return (gethash "sizes" build-config))))
         (*builtin-size-table* (loop with table = (make-hash-table :test 'equal)
                                     for size-info across builtin-class-sizes
                                     for type = (gethash "name" size-info)
                                     for size = (gethash "size" size-info)
                                     do (setf (gethash type table) size)
                                     finally (return table)))
         (builtin-field-offsets (loop for build-config
                                        across (gethash "builtin_class_member_offsets" root)
                                      when (string= "float_64"
                                                    (gethash "build_configuration" build-config))
                                        do (return (gethash "classes" build-config))))
         (*builtin-field-table* (loop with table = (make-hash-table :test 'equal)
                                      for field-info across builtin-field-offsets
                                      for type = (gethash "name" field-info)
                                      for fields = (gethash "members" field-info)
                                      do (setf (gethash type table)
                                               (loop for field-info across fields
                                                     collect (list (gethash "member" field-info)
                                                                   (gethash "offset" field-info)
                                                                   (gethash "meta" field-info))))
                                      finally (return table)))
         (*class-method-table* (make-hash-table :test 'eq))
         (*class-constructor-table* (make-hash-table :test 'eq))
         (*class-destructor-table* (make-hash-table :test 'eq))
         (*class-properties-table* (make-hash-table :test 'eq)))
    (a:with-output-to-file (out *extension-api-bindings-file*
                                :if-exists :supersede)
      (let ((*print-case* :downcase)
            (*print-pretty* t)
            (*package* (find-package :pz-godot-pristine))
            (*exports*))
        (prin1 '(uiop:define-package :%godot (:use)) out)
        (terpri out)
        (prin1 '(cl:in-package :%godot) out)
        (loop for enum-def across (gethash "global_enums" root)
              do (explode-extension-enum out enum-def))
        (loop for class-def across (gethash "builtin_classes" root)
              do (explode-extension-class out class-def :builtin t))
        (explode-extension-class out (a:plist-hash-table '("name" "Variant"
                                                           "api_type" "core"))
                                 :builtin t)
        (loop for class-def across (gethash "classes" root)
              do (explode-extension-class out class-def))
        (loop for struct-def across (gethash "native_structures" root)
              do (explode-native-structures out struct-def))
        (loop for singleton-def across (gethash "singletons" root)
              do (explode-singleton out singleton-def))
        (loop for class-name being the hash-key in *class-constructor-table*
                using (hash-value constructors)
              when constructors
                do (loop for constructor-def across constructors
                         do (explode-extension-constructor out class-name constructor-def)))
        (loop for class-name being the hash-key in *class-destructor-table*
              do (explode-extension-destructor out class-name))
        (loop for class-name being the hash-key in *class-method-table*
                using (hash-value methods)
              when methods
                do (loop for method-def across methods
                         do (explode-extension-method
                             out class-name method-def)))
        (loop for class-name being the hash-key in *class-properties-table*
                using (hash-value properties)
              when properties
                do (loop for property-def across properties
                         do (explode-extension-property out class-name property-def)))
        (terpri out)
        (terpri out)
        (prin1 `(cl:export '(,@(nreverse *exports*))) out)))))


(defun regenerate-bindings ()
  (generate-gdext-bindings)
  (generate-godot-extension-bindings)
  (values))
