(common-lisp:in-package :%godot)


(defgmethod
 (godot-instance+start :class 'godot-instance :bind "start" :hash 2240911060)
 bool)

(defgmethod
 (godot-instance+is-started :class 'godot-instance :bind "is_started" :hash
  2240911060)
 bool)

(defgmethod
 (godot-instance+iteration :class 'godot-instance :bind "iteration" :hash
  2240911060)
 bool)

(defgmethod
 (godot-instance+focus-in :class 'godot-instance :bind "focus_in" :hash
  3218959716)
 :void)

(defgmethod
 (godot-instance+focus-out :class 'godot-instance :bind "focus_out" :hash
  3218959716)
 :void)

(defgmethod
 (godot-instance+pause :class 'godot-instance :bind "pause" :hash 3218959716)
 :void)

(defgmethod
 (godot-instance+resume :class 'godot-instance :bind "resume" :hash 3218959716)
 :void)