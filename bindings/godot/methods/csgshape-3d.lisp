(common-lisp:in-package :%godot)


(defgmethod
 (csgshape-3d+is-root-shape :class 'csgshape-3d :bind "is_root_shape" :hash
  36873697)
 bool)

(defgmethod
 (csgshape-3d+set-operation :class 'csgshape-3d :bind "set_operation" :hash
  811425055)
 :void (operation csgshape-3d+operation))

(defgmethod
 (csgshape-3d+get-operation :class 'csgshape-3d :bind "get_operation" :hash
  2662425879)
 csgshape-3d+operation)

(defgmethod
 (csgshape-3d+set-snap :class 'csgshape-3d :bind "set_snap" :hash 373806689)
 :void (snap float))

(defgmethod
 (csgshape-3d+get-snap :class 'csgshape-3d :bind "get_snap" :hash 1740695150)
 float)

(defgmethod
 (csgshape-3d+set-use-collision :class 'csgshape-3d :bind "set_use_collision"
  :hash 2586408642)
 :void (operation bool))

(defgmethod
 (csgshape-3d+is-using-collision :class 'csgshape-3d :bind "is_using_collision"
  :hash 36873697)
 bool)

(defgmethod
 (csgshape-3d+set-collision-layer :class 'csgshape-3d :bind
  "set_collision_layer" :hash 1286410249)
 :void (layer int))

(defgmethod
 (csgshape-3d+get-collision-layer :class 'csgshape-3d :bind
  "get_collision_layer" :hash 3905245786)
 int)

(defgmethod
 (csgshape-3d+set-collision-mask :class 'csgshape-3d :bind "set_collision_mask"
  :hash 1286410249)
 :void (mask int))

(defgmethod
 (csgshape-3d+get-collision-mask :class 'csgshape-3d :bind "get_collision_mask"
  :hash 3905245786)
 int)

(defgmethod
 (csgshape-3d+set-collision-mask-value :class 'csgshape-3d :bind
  "set_collision_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (csgshape-3d+get-collision-mask-value :class 'csgshape-3d :bind
  "get_collision_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (csgshape-3d+set-collision-layer-value :class 'csgshape-3d :bind
  "set_collision_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (csgshape-3d+get-collision-layer-value :class 'csgshape-3d :bind
  "get_collision_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (csgshape-3d+set-collision-priority :class 'csgshape-3d :bind
  "set_collision_priority" :hash 373806689)
 :void (priority float))

(defgmethod
 (csgshape-3d+get-collision-priority :class 'csgshape-3d :bind
  "get_collision_priority" :hash 1740695150)
 float)

(defgmethod
 (csgshape-3d+bake-collision-shape :class 'csgshape-3d :bind
  "bake_collision_shape" :hash 36102322)
 concave-polygon-shape-3d)

(defgmethod
 (csgshape-3d+set-calculate-tangents :class 'csgshape-3d :bind
  "set_calculate_tangents" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (csgshape-3d+is-calculating-tangents :class 'csgshape-3d :bind
  "is_calculating_tangents" :hash 36873697)
 bool)

(defgmethod
 (csgshape-3d+get-meshes :class 'csgshape-3d :bind "get_meshes" :hash
  3995934104)
 array)

(defgmethod
 (csgshape-3d+bake-static-mesh :class 'csgshape-3d :bind "bake_static_mesh"
  :hash 1605880883)
 array-mesh)

(defgmethod
 (csgshape-3d+set-autosmooth :class 'csgshape-3d :bind "set_autosmooth" :hash
  2586408642)
 :void (autosmooth bool))

(defgmethod
 (csgshape-3d+is-autosmooth :class 'csgshape-3d :bind "is_autosmooth" :hash
  36873697)
 bool)

(defgmethod
 (csgshape-3d+set-smoothing-angle :class 'csgshape-3d :bind
  "set_smoothing_angle" :hash 373806689)
 :void (smoothing-angle float))

(defgmethod
 (csgshape-3d+get-smoothing-angle :class 'csgshape-3d :bind
  "get_smoothing_angle" :hash 1740695150)
 float)