(common-lisp:in-package :%godot)


(defgmethod
 (slider+set-ticks :class 'slider :bind "set_ticks" :hash 1286410249) :void
 (count int))

(defgmethod
 (slider+get-ticks :class 'slider :bind "get_ticks" :hash 3905245786) int)

(defgmethod
 (slider+get-ticks-on-borders :class 'slider :bind "get_ticks_on_borders" :hash
  36873697)
 bool)

(defgmethod
 (slider+set-ticks-on-borders :class 'slider :bind "set_ticks_on_borders" :hash
  2586408642)
 :void (ticks-on-border bool))

(defgmethod
 (slider+get-ticks-position :class 'slider :bind "get_ticks_position" :hash
  3567635531)
 slider+tick-position)

(defgmethod
 (slider+set-ticks-position :class 'slider :bind "set_ticks_position" :hash
  2952822224)
 :void (ticks-on-border slider+tick-position))

(defgmethod
 (slider+set-editable :class 'slider :bind "set_editable" :hash 2586408642)
 :void (editable bool))

(defgmethod
 (slider+is-editable :class 'slider :bind "is_editable" :hash 36873697) bool)

(defgmethod
 (slider+set-scrollable :class 'slider :bind "set_scrollable" :hash 2586408642)
 :void (scrollable bool))

(defgmethod
 (slider+is-scrollable :class 'slider :bind "is_scrollable" :hash 36873697)
 bool)