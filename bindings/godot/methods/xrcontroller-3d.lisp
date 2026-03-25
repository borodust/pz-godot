(common-lisp:in-package :%godot)


(defgmethod
 (xrcontroller-3d+is-button-pressed :class 'xrcontroller-3d :bind
  "is_button_pressed" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (xrcontroller-3d+get-input :class 'xrcontroller-3d :bind "get_input" :hash
  2760726917)
 variant (name string-name))

(defgmethod
 (xrcontroller-3d+get-float :class 'xrcontroller-3d :bind "get_float" :hash
  2349060816)
 float (name string-name))

(defgmethod
 (xrcontroller-3d+get-vector2 :class 'xrcontroller-3d :bind "get_vector2" :hash
  3100822709)
 vector-2 (name string-name))

(defgmethod
 (xrcontroller-3d+get-tracker-hand :class 'xrcontroller-3d :bind
  "get_tracker_hand" :hash 4181770860)
 xrpositional-tracker+tracker-hand)