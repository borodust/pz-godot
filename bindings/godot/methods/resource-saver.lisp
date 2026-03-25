(common-lisp:in-package :%godot)


(defgmethod
 (resource-saver+save :class 'resource-saver :bind "save" :hash 2983274697)
 error (resource resource) (path string) (flags resource-saver+saver-flags))

(defgmethod
 (resource-saver+set-uid :class 'resource-saver :bind "set_uid" :hash
  993915709)
 error (resource string) (uid int))

(defgmethod
 (resource-saver+get-recognized-extensions :class 'resource-saver :bind
  "get_recognized_extensions" :hash 4223597960)
 packed-string-array (type resource))

(defgmethod
 (resource-saver+add-resource-format-saver :class 'resource-saver :bind
  "add_resource_format_saver" :hash 362894272)
 :void (format-saver resource-format-saver) (at-front bool))

(defgmethod
 (resource-saver+remove-resource-format-saver :class 'resource-saver :bind
  "remove_resource_format_saver" :hash 3373026878)
 :void (format-saver resource-format-saver))

(defgmethod
 (resource-saver+get-resource-id-for-path :class 'resource-saver :bind
  "get_resource_id_for_path" :hash 150756522)
 int (path string) (generate bool))