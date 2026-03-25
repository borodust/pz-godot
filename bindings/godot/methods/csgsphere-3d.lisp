(common-lisp:in-package :%godot)


(defgmethod
 (csgsphere-3d+set-radius :class 'csgsphere-3d :bind "set_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (csgsphere-3d+get-radius :class 'csgsphere-3d :bind "get_radius" :hash
  1740695150)
 float)

(defgmethod
 (csgsphere-3d+set-radial-segments :class 'csgsphere-3d :bind
  "set_radial_segments" :hash 1286410249)
 :void (radial-segments int))

(defgmethod
 (csgsphere-3d+get-radial-segments :class 'csgsphere-3d :bind
  "get_radial_segments" :hash 3905245786)
 int)

(defgmethod
 (csgsphere-3d+set-rings :class 'csgsphere-3d :bind "set_rings" :hash
  1286410249)
 :void (rings int))

(defgmethod
 (csgsphere-3d+get-rings :class 'csgsphere-3d :bind "get_rings" :hash
  3905245786)
 int)

(defgmethod
 (csgsphere-3d+set-smooth-faces :class 'csgsphere-3d :bind "set_smooth_faces"
  :hash 2586408642)
 :void (smooth-faces bool))

(defgmethod
 (csgsphere-3d+get-smooth-faces :class 'csgsphere-3d :bind "get_smooth_faces"
  :hash 36873697)
 bool)

(defgmethod
 (csgsphere-3d+set-material :class 'csgsphere-3d :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (csgsphere-3d+get-material :class 'csgsphere-3d :bind "get_material" :hash
  5934680)
 material)