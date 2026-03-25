(common-lisp:in-package :%godot)


(defgmethod
 (torus-mesh+set-inner-radius :class 'torus-mesh :bind "set_inner_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (torus-mesh+get-inner-radius :class 'torus-mesh :bind "get_inner_radius" :hash
  1740695150)
 float)

(defgmethod
 (torus-mesh+set-outer-radius :class 'torus-mesh :bind "set_outer_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (torus-mesh+get-outer-radius :class 'torus-mesh :bind "get_outer_radius" :hash
  1740695150)
 float)

(defgmethod
 (torus-mesh+set-rings :class 'torus-mesh :bind "set_rings" :hash 1286410249)
 :void (rings int))

(defgmethod
 (torus-mesh+get-rings :class 'torus-mesh :bind "get_rings" :hash 3905245786)
 int)

(defgmethod
 (torus-mesh+set-ring-segments :class 'torus-mesh :bind "set_ring_segments"
  :hash 1286410249)
 :void (rings int))

(defgmethod
 (torus-mesh+get-ring-segments :class 'torus-mesh :bind "get_ring_segments"
  :hash 3905245786)
 int)