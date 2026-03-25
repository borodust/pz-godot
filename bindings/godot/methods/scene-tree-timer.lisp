(common-lisp:in-package :%godot)


(defgmethod
 (scene-tree-timer+set-time-left :class 'scene-tree-timer :bind "set_time_left"
  :hash 373806689)
 :void (time float))

(defgmethod
 (scene-tree-timer+get-time-left :class 'scene-tree-timer :bind "get_time_left"
  :hash 1740695150)
 float)