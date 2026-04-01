(common-lisp:in-package :%godot)


(defgmethod
 (resource-format-loader+%get-recognized-extensions :class
  'resource-format-loader :bind "_get_recognized_extensions" :hash 1139954409
  :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (resource-format-loader+%recognize-path :class 'resource-format-loader :bind
  "_recognize_path" :hash 2594487047 :virtual common-lisp:t)
 bool (path string) (type string-name))

(defgmethod
 (resource-format-loader+%handles-type :class 'resource-format-loader :bind
  "_handles_type" :hash 2619796661 :virtual common-lisp:t)
 bool (type string-name))

(defgmethod
 (resource-format-loader+%get-resource-type :class 'resource-format-loader
  :bind "_get_resource_type" :hash 3135753539 :virtual common-lisp:t)
 string (path string))

(defgmethod
 (resource-format-loader+%get-resource-script-class :class
  'resource-format-loader :bind "_get_resource_script_class" :hash 3135753539
  :virtual common-lisp:t)
 string (path string))

(defgmethod
 (resource-format-loader+%get-resource-uid :class 'resource-format-loader :bind
  "_get_resource_uid" :hash 1321353865 :virtual common-lisp:t)
 int (path string))

(defgmethod
 (resource-format-loader+%get-dependencies :class 'resource-format-loader :bind
  "_get_dependencies" :hash 6257701 :virtual common-lisp:t)
 packed-string-array (path string) (add-types bool))

(defgmethod
 (resource-format-loader+%rename-dependencies :class 'resource-format-loader
  :bind "_rename_dependencies" :hash 223715120 :virtual common-lisp:t)
 error (path string) (renames dictionary))

(defgmethod
 (resource-format-loader+%exists :class 'resource-format-loader :bind "_exists"
  :hash 3927539163 :virtual common-lisp:t)
 bool (path string))

(defgmethod
 (resource-format-loader+%get-classes-used :class 'resource-format-loader :bind
  "_get_classes_used" :hash 4291131558 :virtual common-lisp:t)
 packed-string-array (path string))

(defgmethod
 (resource-format-loader+%load :class 'resource-format-loader :bind "_load"
  :hash 2885906527 :virtual common-lisp:t)
 variant (path string) (original-path string) (use-sub-threads bool)
 (cache-mode int))