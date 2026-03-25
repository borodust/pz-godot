(common-lisp:in-package :%godot)


(defgmethod
 (packed-scene+pack :class 'packed-scene :bind "pack" :hash 2584678054) error
 (path node))

(defgmethod
 (packed-scene+instantiate :class 'packed-scene :bind "instantiate" :hash
  2628778455)
 node (edit-state packed-scene+gen-edit-state))

(defgmethod
 (packed-scene+can-instantiate :class 'packed-scene :bind "can_instantiate"
  :hash 36873697)
 bool)

(defgmethod
 (packed-scene+get-state :class 'packed-scene :bind "get_state" :hash
  3479783971)
 scene-state)