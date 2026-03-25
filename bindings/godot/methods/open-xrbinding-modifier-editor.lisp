(common-lisp:in-package :%godot)


(defgmethod
 (open-xrbinding-modifier-editor+get-binding-modifier :class
  'open-xrbinding-modifier-editor :bind "get_binding_modifier" :hash
  2930765082)
 open-xrbinding-modifier)

(defgmethod
 (open-xrbinding-modifier-editor+setup :class 'open-xrbinding-modifier-editor
  :bind "setup" :hash 1284787389)
 :void (action-map open-xraction-map)
 (binding-modifier open-xrbinding-modifier))