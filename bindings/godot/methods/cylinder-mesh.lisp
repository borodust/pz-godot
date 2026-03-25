(common-lisp:in-package :%godot)


(defgmethod
 (cylinder-mesh+set-top-radius :class 'cylinder-mesh :bind "set_top_radius"
  :hash 373806689)
 :void (radius float))

(defgmethod
 (cylinder-mesh+get-top-radius :class 'cylinder-mesh :bind "get_top_radius"
  :hash 1740695150)
 float)

(defgmethod
 (cylinder-mesh+set-bottom-radius :class 'cylinder-mesh :bind
  "set_bottom_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (cylinder-mesh+get-bottom-radius :class 'cylinder-mesh :bind
  "get_bottom_radius" :hash 1740695150)
 float)

(defgmethod
 (cylinder-mesh+set-height :class 'cylinder-mesh :bind "set_height" :hash
  373806689)
 :void (height float))

(defgmethod
 (cylinder-mesh+get-height :class 'cylinder-mesh :bind "get_height" :hash
  1740695150)
 float)

(defgmethod
 (cylinder-mesh+set-radial-segments :class 'cylinder-mesh :bind
  "set_radial_segments" :hash 1286410249)
 :void (segments int))

(defgmethod
 (cylinder-mesh+get-radial-segments :class 'cylinder-mesh :bind
  "get_radial_segments" :hash 3905245786)
 int)

(defgmethod
 (cylinder-mesh+set-rings :class 'cylinder-mesh :bind "set_rings" :hash
  1286410249)
 :void (rings int))

(defgmethod
 (cylinder-mesh+get-rings :class 'cylinder-mesh :bind "get_rings" :hash
  3905245786)
 int)

(defgmethod
 (cylinder-mesh+set-cap-top :class 'cylinder-mesh :bind "set_cap_top" :hash
  2586408642)
 :void (cap-top bool))

(defgmethod
 (cylinder-mesh+is-cap-top :class 'cylinder-mesh :bind "is_cap_top" :hash
  36873697)
 bool)

(defgmethod
 (cylinder-mesh+set-cap-bottom :class 'cylinder-mesh :bind "set_cap_bottom"
  :hash 2586408642)
 :void (cap-bottom bool))

(defgmethod
 (cylinder-mesh+is-cap-bottom :class 'cylinder-mesh :bind "is_cap_bottom" :hash
  36873697)
 bool)