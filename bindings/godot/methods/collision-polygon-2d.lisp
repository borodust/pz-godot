(common-lisp:in-package :%godot)


(defgmethod
 (collision-polygon-2d+set-polygon :class 'collision-polygon-2d :bind
  "set_polygon" :hash 1509147220)
 :void (polygon packed-vector-2array))

(defgmethod
 (collision-polygon-2d+get-polygon :class 'collision-polygon-2d :bind
  "get_polygon" :hash 2961356807)
 packed-vector-2array)

(defgmethod
 (collision-polygon-2d+set-build-mode :class 'collision-polygon-2d :bind
  "set_build_mode" :hash 2780803135)
 :void (build-mode collision-polygon-2d+build-mode))

(defgmethod
 (collision-polygon-2d+get-build-mode :class 'collision-polygon-2d :bind
  "get_build_mode" :hash 3044948800)
 collision-polygon-2d+build-mode)

(defgmethod
 (collision-polygon-2d+set-disabled :class 'collision-polygon-2d :bind
  "set_disabled" :hash 2586408642)
 :void (disabled bool))

(defgmethod
 (collision-polygon-2d+is-disabled :class 'collision-polygon-2d :bind
  "is_disabled" :hash 36873697)
 bool)

(defgmethod
 (collision-polygon-2d+set-one-way-collision :class 'collision-polygon-2d :bind
  "set_one_way_collision" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (collision-polygon-2d+is-one-way-collision-enabled :class
  'collision-polygon-2d :bind "is_one_way_collision_enabled" :hash 36873697)
 bool)

(defgmethod
 (collision-polygon-2d+set-one-way-collision-margin :class
  'collision-polygon-2d :bind "set_one_way_collision_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (collision-polygon-2d+get-one-way-collision-margin :class
  'collision-polygon-2d :bind "get_one_way_collision_margin" :hash 1740695150)
 float)

(defgmethod
 (collision-polygon-2d+set-one-way-collision-direction :class
  'collision-polygon-2d :bind "set_one_way_collision_direction" :hash
  743155724)
 :void (direction vector-2))

(defgmethod
 (collision-polygon-2d+get-one-way-collision-direction :class
  'collision-polygon-2d :bind "get_one_way_collision_direction" :hash
  3341600327)
 vector-2)