(common-lisp:in-package :%godot)


(defgmethod
 (occluder-instance-3d+set-bake-mask :class 'occluder-instance-3d :bind
  "set_bake_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (occluder-instance-3d+get-bake-mask :class 'occluder-instance-3d :bind
  "get_bake_mask" :hash 3905245786)
 int)

(defgmethod
 (occluder-instance-3d+set-bake-mask-value :class 'occluder-instance-3d :bind
  "set_bake_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (occluder-instance-3d+get-bake-mask-value :class 'occluder-instance-3d :bind
  "get_bake_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (occluder-instance-3d+set-bake-simplification-distance :class
  'occluder-instance-3d :bind "set_bake_simplification_distance" :hash
  373806689)
 :void (simplification-distance float))

(defgmethod
 (occluder-instance-3d+get-bake-simplification-distance :class
  'occluder-instance-3d :bind "get_bake_simplification_distance" :hash
  1740695150)
 float)

(defgmethod
 (occluder-instance-3d+set-occluder :class 'occluder-instance-3d :bind
  "set_occluder" :hash 1664878165)
 :void (occluder occluder-3d))

(defgmethod
 (occluder-instance-3d+get-occluder :class 'occluder-instance-3d :bind
  "get_occluder" :hash 1696836198)
 occluder-3d)