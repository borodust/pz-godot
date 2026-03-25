(common-lisp:in-package :%godot)


(defgmethod
 (groove-joint-2d+set-length :class 'groove-joint-2d :bind "set_length" :hash
  373806689)
 :void (length float))

(defgmethod
 (groove-joint-2d+get-length :class 'groove-joint-2d :bind "get_length" :hash
  1740695150)
 float)

(defgmethod
 (groove-joint-2d+set-initial-offset :class 'groove-joint-2d :bind
  "set_initial_offset" :hash 373806689)
 :void (offset float))

(defgmethod
 (groove-joint-2d+get-initial-offset :class 'groove-joint-2d :bind
  "get_initial_offset" :hash 1740695150)
 float)