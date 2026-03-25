(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-capability-configuration-april-tag+get-enabled-components
  :class 'open-xrspatial-capability-configuration-april-tag :bind
  "get_enabled_components" :hash 235988956)
 packed-int-64array)

(defgmethod
 (open-xrspatial-capability-configuration-april-tag+set-april-dict :class
  'open-xrspatial-capability-configuration-april-tag :bind "set_april_dict"
  :hash 3902905799)
 :void
 (april-dict open-xrspatial-capability-configuration-april-tag+april-tag-dict))

(defgmethod
 (open-xrspatial-capability-configuration-april-tag+get-april-dict :class
  'open-xrspatial-capability-configuration-april-tag :bind "get_april_dict"
  :hash 440273016)
 open-xrspatial-capability-configuration-april-tag+april-tag-dict)