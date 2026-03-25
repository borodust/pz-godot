(common-lisp:in-package :%godot)


(defgmethod
 (capsule-shape-2d+set-radius :class 'capsule-shape-2d :bind "set_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (capsule-shape-2d+get-radius :class 'capsule-shape-2d :bind "get_radius" :hash
  1740695150)
 float)

(defgmethod
 (capsule-shape-2d+set-height :class 'capsule-shape-2d :bind "set_height" :hash
  373806689)
 :void (height float))

(defgmethod
 (capsule-shape-2d+get-height :class 'capsule-shape-2d :bind "get_height" :hash
  1740695150)
 float)

(defgmethod
 (capsule-shape-2d+set-mid-height :class 'capsule-shape-2d :bind
  "set_mid_height" :hash 373806689)
 :void (mid-height float))

(defgmethod
 (capsule-shape-2d+get-mid-height :class 'capsule-shape-2d :bind
  "get_mid_height" :hash 1740695150)
 float)