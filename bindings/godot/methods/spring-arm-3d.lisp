(common-lisp:in-package :%godot)


(defgmethod
 (spring-arm-3d+get-hit-length :class 'spring-arm-3d :bind "get_hit_length"
  :hash 191475506)
 float)

(defgmethod
 (spring-arm-3d+set-length :class 'spring-arm-3d :bind "set_length" :hash
  373806689)
 :void (length float))

(defgmethod
 (spring-arm-3d+get-length :class 'spring-arm-3d :bind "get_length" :hash
  1740695150)
 float)

(defgmethod
 (spring-arm-3d+set-shape :class 'spring-arm-3d :bind "set_shape" :hash
  1549710052)
 :void (shape shape-3d))

(defgmethod
 (spring-arm-3d+get-shape :class 'spring-arm-3d :bind "get_shape" :hash
  3214262478)
 shape-3d)

(defgmethod
 (spring-arm-3d+add-excluded-object :class 'spring-arm-3d :bind
  "add_excluded_object" :hash 2722037293)
 :void (rid rid))

(defgmethod
 (spring-arm-3d+remove-excluded-object :class 'spring-arm-3d :bind
  "remove_excluded_object" :hash 3521089500)
 bool (rid rid))

(defgmethod
 (spring-arm-3d+clear-excluded-objects :class 'spring-arm-3d :bind
  "clear_excluded_objects" :hash 3218959716)
 :void)

(defgmethod
 (spring-arm-3d+set-collision-mask :class 'spring-arm-3d :bind
  "set_collision_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (spring-arm-3d+get-collision-mask :class 'spring-arm-3d :bind
  "get_collision_mask" :hash 2455072627)
 int)

(defgmethod
 (spring-arm-3d+set-margin :class 'spring-arm-3d :bind "set_margin" :hash
  373806689)
 :void (margin float))

(defgmethod
 (spring-arm-3d+get-margin :class 'spring-arm-3d :bind "get_margin" :hash
  191475506)
 float)