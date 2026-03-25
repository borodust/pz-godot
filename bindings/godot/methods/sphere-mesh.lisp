(common-lisp:in-package :%godot)


(defgmethod
 (sphere-mesh+set-radius :class 'sphere-mesh :bind "set_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (sphere-mesh+get-radius :class 'sphere-mesh :bind "get_radius" :hash
  1740695150)
 float)

(defgmethod
 (sphere-mesh+set-height :class 'sphere-mesh :bind "set_height" :hash
  373806689)
 :void (height float))

(defgmethod
 (sphere-mesh+get-height :class 'sphere-mesh :bind "get_height" :hash
  1740695150)
 float)

(defgmethod
 (sphere-mesh+set-radial-segments :class 'sphere-mesh :bind
  "set_radial_segments" :hash 1286410249)
 :void (radial-segments int))

(defgmethod
 (sphere-mesh+get-radial-segments :class 'sphere-mesh :bind
  "get_radial_segments" :hash 3905245786)
 int)

(defgmethod
 (sphere-mesh+set-rings :class 'sphere-mesh :bind "set_rings" :hash 1286410249)
 :void (rings int))

(defgmethod
 (sphere-mesh+get-rings :class 'sphere-mesh :bind "get_rings" :hash 3905245786)
 int)

(defgmethod
 (sphere-mesh+set-is-hemisphere :class 'sphere-mesh :bind "set_is_hemisphere"
  :hash 2586408642)
 :void (is-hemisphere bool))

(defgmethod
 (sphere-mesh+get-is-hemisphere :class 'sphere-mesh :bind "get_is_hemisphere"
  :hash 36873697)
 bool)