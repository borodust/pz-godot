(common-lisp:in-package :%godot)


(defgmethod
 (csgcylinder-3d+set-radius :class 'csgcylinder-3d :bind "set_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (csgcylinder-3d+get-radius :class 'csgcylinder-3d :bind "get_radius" :hash
  1740695150)
 float)

(defgmethod
 (csgcylinder-3d+set-height :class 'csgcylinder-3d :bind "set_height" :hash
  373806689)
 :void (height float))

(defgmethod
 (csgcylinder-3d+get-height :class 'csgcylinder-3d :bind "get_height" :hash
  1740695150)
 float)

(defgmethod
 (csgcylinder-3d+set-sides :class 'csgcylinder-3d :bind "set_sides" :hash
  1286410249)
 :void (sides int))

(defgmethod
 (csgcylinder-3d+get-sides :class 'csgcylinder-3d :bind "get_sides" :hash
  3905245786)
 int)

(defgmethod
 (csgcylinder-3d+set-cone :class 'csgcylinder-3d :bind "set_cone" :hash
  2586408642)
 :void (cone bool))

(defgmethod
 (csgcylinder-3d+is-cone :class 'csgcylinder-3d :bind "is_cone" :hash 36873697)
 bool)

(defgmethod
 (csgcylinder-3d+set-material :class 'csgcylinder-3d :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (csgcylinder-3d+get-material :class 'csgcylinder-3d :bind "get_material" :hash
  5934680)
 material)

(defgmethod
 (csgcylinder-3d+set-smooth-faces :class 'csgcylinder-3d :bind
  "set_smooth_faces" :hash 2586408642)
 :void (smooth-faces bool))

(defgmethod
 (csgcylinder-3d+get-smooth-faces :class 'csgcylinder-3d :bind
  "get_smooth_faces" :hash 36873697)
 bool)