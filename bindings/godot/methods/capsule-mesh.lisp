(common-lisp:in-package :%godot)


(defgmethod
 (capsule-mesh+set-radius :class 'capsule-mesh :bind "set_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (capsule-mesh+get-radius :class 'capsule-mesh :bind "get_radius" :hash
  1740695150)
 float)

(defgmethod
 (capsule-mesh+set-height :class 'capsule-mesh :bind "set_height" :hash
  373806689)
 :void (height float))

(defgmethod
 (capsule-mesh+get-height :class 'capsule-mesh :bind "get_height" :hash
  1740695150)
 float)

(defgmethod
 (capsule-mesh+set-radial-segments :class 'capsule-mesh :bind
  "set_radial_segments" :hash 1286410249)
 :void (segments int))

(defgmethod
 (capsule-mesh+get-radial-segments :class 'capsule-mesh :bind
  "get_radial_segments" :hash 3905245786)
 int)

(defgmethod
 (capsule-mesh+set-rings :class 'capsule-mesh :bind "set_rings" :hash
  1286410249)
 :void (rings int))

(defgmethod
 (capsule-mesh+get-rings :class 'capsule-mesh :bind "get_rings" :hash
  3905245786)
 int)