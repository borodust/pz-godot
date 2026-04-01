(common-lisp:in-package :%godot)


(defgmethod
 (resource-format-saver+%save :class 'resource-format-saver :bind "_save" :hash
  2794699034 :virtual common-lisp:t)
 error (resource resource) (path string) (flags int))

(defgmethod
 (resource-format-saver+%set-uid :class 'resource-format-saver :bind "_set_uid"
  :hash 993915709 :virtual common-lisp:t)
 error (path string) (uid int))

(defgmethod
 (resource-format-saver+%recognize :class 'resource-format-saver :bind
  "_recognize" :hash 3190994482 :virtual common-lisp:t)
 bool (resource resource))

(defgmethod
 (resource-format-saver+%get-recognized-extensions :class
  'resource-format-saver :bind "_get_recognized_extensions" :hash 1567505034
  :virtual common-lisp:t)
 packed-string-array (resource resource))

(defgmethod
 (resource-format-saver+%recognize-path :class 'resource-format-saver :bind
  "_recognize_path" :hash 710996192 :virtual common-lisp:t)
 bool (resource resource) (path string))