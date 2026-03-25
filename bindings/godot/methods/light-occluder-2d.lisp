(common-lisp:in-package :%godot)


(defgmethod
 (light-occluder-2d+set-occluder-polygon :class 'light-occluder-2d :bind
  "set_occluder_polygon" :hash 3258315893)
 :void (polygon occluder-polygon-2d))

(defgmethod
 (light-occluder-2d+get-occluder-polygon :class 'light-occluder-2d :bind
  "get_occluder_polygon" :hash 3962317075)
 occluder-polygon-2d)

(defgmethod
 (light-occluder-2d+set-occluder-light-mask :class 'light-occluder-2d :bind
  "set_occluder_light_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (light-occluder-2d+get-occluder-light-mask :class 'light-occluder-2d :bind
  "get_occluder_light_mask" :hash 3905245786)
 int)

(defgmethod
 (light-occluder-2d+set-as-sdf-collision :class 'light-occluder-2d :bind
  "set_as_sdf_collision" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (light-occluder-2d+is-set-as-sdf-collision :class 'light-occluder-2d :bind
  "is_set_as_sdf_collision" :hash 36873697)
 bool)