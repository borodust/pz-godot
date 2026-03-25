(common-lisp:in-package :%godot)


(defgmethod
 (spring-bone-collision-sphere-3d+set-radius :class
  'spring-bone-collision-sphere-3d :bind "set_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (spring-bone-collision-sphere-3d+get-radius :class
  'spring-bone-collision-sphere-3d :bind "get_radius" :hash 1740695150)
 float)

(defgmethod
 (spring-bone-collision-sphere-3d+set-inside :class
  'spring-bone-collision-sphere-3d :bind "set_inside" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (spring-bone-collision-sphere-3d+is-inside :class
  'spring-bone-collision-sphere-3d :bind "is_inside" :hash 36873697)
 bool)