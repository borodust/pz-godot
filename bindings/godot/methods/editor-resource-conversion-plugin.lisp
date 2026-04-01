(common-lisp:in-package :%godot)


(defgmethod
 (editor-resource-conversion-plugin+%converts-to :class
  'editor-resource-conversion-plugin :bind "_converts_to" :hash 201670096
  :virtual common-lisp:t)
 string)

(defgmethod
 (editor-resource-conversion-plugin+%handles :class
  'editor-resource-conversion-plugin :bind "_handles" :hash 3190994482 :virtual
  common-lisp:t)
 bool (resource resource))

(defgmethod
 (editor-resource-conversion-plugin+%convert :class
  'editor-resource-conversion-plugin :bind "_convert" :hash 325183270 :virtual
  common-lisp:t)
 resource (resource resource))