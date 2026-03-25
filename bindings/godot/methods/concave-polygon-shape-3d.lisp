(common-lisp:in-package :%godot)


(defgmethod
 (concave-polygon-shape-3d+set-faces :class 'concave-polygon-shape-3d :bind
  "set_faces" :hash 334873810)
 :void (faces packed-vector-3array))

(defgmethod
 (concave-polygon-shape-3d+get-faces :class 'concave-polygon-shape-3d :bind
  "get_faces" :hash 497664490)
 packed-vector-3array)

(defgmethod
 (concave-polygon-shape-3d+set-backface-collision-enabled :class
  'concave-polygon-shape-3d :bind "set_backface_collision_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (concave-polygon-shape-3d+is-backface-collision-enabled :class
  'concave-polygon-shape-3d :bind "is_backface_collision_enabled" :hash
  36873697)
 bool)