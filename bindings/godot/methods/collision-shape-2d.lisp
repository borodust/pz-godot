(common-lisp:in-package :%godot)


(defgmethod
 (collision-shape-2d+set-shape :class 'collision-shape-2d :bind "set_shape"
  :hash 771364740)
 :void (shape shape-2d))

(defgmethod
 (collision-shape-2d+get-shape :class 'collision-shape-2d :bind "get_shape"
  :hash 522005891)
 shape-2d)

(defgmethod
 (collision-shape-2d+set-disabled :class 'collision-shape-2d :bind
  "set_disabled" :hash 2586408642)
 :void (disabled bool))

(defgmethod
 (collision-shape-2d+is-disabled :class 'collision-shape-2d :bind "is_disabled"
  :hash 36873697)
 bool)

(defgmethod
 (collision-shape-2d+set-one-way-collision :class 'collision-shape-2d :bind
  "set_one_way_collision" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (collision-shape-2d+is-one-way-collision-enabled :class 'collision-shape-2d
  :bind "is_one_way_collision_enabled" :hash 36873697)
 bool)

(defgmethod
 (collision-shape-2d+set-one-way-collision-margin :class 'collision-shape-2d
  :bind "set_one_way_collision_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (collision-shape-2d+get-one-way-collision-margin :class 'collision-shape-2d
  :bind "get_one_way_collision_margin" :hash 1740695150)
 float)

(defgmethod
 (collision-shape-2d+set-one-way-collision-direction :class 'collision-shape-2d
  :bind "set_one_way_collision_direction" :hash 743155724)
 :void (direction vector-2))

(defgmethod
 (collision-shape-2d+get-one-way-collision-direction :class 'collision-shape-2d
  :bind "get_one_way_collision_direction" :hash 3341600327)
 vector-2)

(defgmethod
 (collision-shape-2d+set-debug-color :class 'collision-shape-2d :bind
  "set_debug_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (collision-shape-2d+get-debug-color :class 'collision-shape-2d :bind
  "get_debug_color" :hash 3444240500)
 color)