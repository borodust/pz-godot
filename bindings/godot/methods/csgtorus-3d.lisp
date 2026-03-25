(common-lisp:in-package :%godot)


(defgmethod
 (csgtorus-3d+set-inner-radius :class 'csgtorus-3d :bind "set_inner_radius"
  :hash 373806689)
 :void (radius float))

(defgmethod
 (csgtorus-3d+get-inner-radius :class 'csgtorus-3d :bind "get_inner_radius"
  :hash 1740695150)
 float)

(defgmethod
 (csgtorus-3d+set-outer-radius :class 'csgtorus-3d :bind "set_outer_radius"
  :hash 373806689)
 :void (radius float))

(defgmethod
 (csgtorus-3d+get-outer-radius :class 'csgtorus-3d :bind "get_outer_radius"
  :hash 1740695150)
 float)

(defgmethod
 (csgtorus-3d+set-sides :class 'csgtorus-3d :bind "set_sides" :hash 1286410249)
 :void (sides int))

(defgmethod
 (csgtorus-3d+get-sides :class 'csgtorus-3d :bind "get_sides" :hash 3905245786)
 int)

(defgmethod
 (csgtorus-3d+set-ring-sides :class 'csgtorus-3d :bind "set_ring_sides" :hash
  1286410249)
 :void (sides int))

(defgmethod
 (csgtorus-3d+get-ring-sides :class 'csgtorus-3d :bind "get_ring_sides" :hash
  3905245786)
 int)

(defgmethod
 (csgtorus-3d+set-material :class 'csgtorus-3d :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (csgtorus-3d+get-material :class 'csgtorus-3d :bind "get_material" :hash
  5934680)
 material)

(defgmethod
 (csgtorus-3d+set-smooth-faces :class 'csgtorus-3d :bind "set_smooth_faces"
  :hash 2586408642)
 :void (smooth-faces bool))

(defgmethod
 (csgtorus-3d+get-smooth-faces :class 'csgtorus-3d :bind "get_smooth_faces"
  :hash 36873697)
 bool)