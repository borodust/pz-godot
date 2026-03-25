(common-lisp:in-package :%godot)


(defgmethod
 (collision-object-3d+-input-event :class 'collision-object-3d :bind
  "_input_event" :hash 2310605070 :virtual common-lisp:t)
 :void (camera camera-3d) (event input-event) (event-position vector-3)
 (normal vector-3) (shape-idx int))

(defgmethod
 (collision-object-3d+-mouse-enter :class 'collision-object-3d :bind
  "_mouse_enter" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (collision-object-3d+-mouse-exit :class 'collision-object-3d :bind
  "_mouse_exit" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (collision-object-3d+set-collision-layer :class 'collision-object-3d :bind
  "set_collision_layer" :hash 1286410249)
 :void (layer int))

(defgmethod
 (collision-object-3d+get-collision-layer :class 'collision-object-3d :bind
  "get_collision_layer" :hash 3905245786)
 int)

(defgmethod
 (collision-object-3d+set-collision-mask :class 'collision-object-3d :bind
  "set_collision_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (collision-object-3d+get-collision-mask :class 'collision-object-3d :bind
  "get_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (collision-object-3d+set-collision-layer-value :class 'collision-object-3d
  :bind "set_collision_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (collision-object-3d+get-collision-layer-value :class 'collision-object-3d
  :bind "get_collision_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (collision-object-3d+set-collision-mask-value :class 'collision-object-3d
  :bind "set_collision_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (collision-object-3d+get-collision-mask-value :class 'collision-object-3d
  :bind "get_collision_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (collision-object-3d+set-collision-priority :class 'collision-object-3d :bind
  "set_collision_priority" :hash 373806689)
 :void (priority float))

(defgmethod
 (collision-object-3d+get-collision-priority :class 'collision-object-3d :bind
  "get_collision_priority" :hash 1740695150)
 float)

(defgmethod
 (collision-object-3d+set-disable-mode :class 'collision-object-3d :bind
  "set_disable_mode" :hash 1623620376)
 :void (mode collision-object-3d+disable-mode))

(defgmethod
 (collision-object-3d+get-disable-mode :class 'collision-object-3d :bind
  "get_disable_mode" :hash 410164780)
 collision-object-3d+disable-mode)

(defgmethod
 (collision-object-3d+set-ray-pickable :class 'collision-object-3d :bind
  "set_ray_pickable" :hash 2586408642)
 :void (ray-pickable bool))

(defgmethod
 (collision-object-3d+is-ray-pickable :class 'collision-object-3d :bind
  "is_ray_pickable" :hash 36873697)
 bool)

(defgmethod
 (collision-object-3d+set-capture-input-on-drag :class 'collision-object-3d
  :bind "set_capture_input_on_drag" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (collision-object-3d+get-capture-input-on-drag :class 'collision-object-3d
  :bind "get_capture_input_on_drag" :hash 36873697)
 bool)

(defgmethod
 (collision-object-3d+get-rid :class 'collision-object-3d :bind "get_rid" :hash
  2944877500)
 rid)

(defgmethod
 (collision-object-3d+create-shape-owner :class 'collision-object-3d :bind
  "create_shape_owner" :hash 3429307534)
 int (owner object))

(defgmethod
 (collision-object-3d+remove-shape-owner :class 'collision-object-3d :bind
  "remove_shape_owner" :hash 1286410249)
 :void (owner-id int))

(defgmethod
 (collision-object-3d+get-shape-owners :class 'collision-object-3d :bind
  "get_shape_owners" :hash 969006518)
 packed-int-32array)

(defgmethod
 (collision-object-3d+shape-owner-set-transform :class 'collision-object-3d
  :bind "shape_owner_set_transform" :hash 3616898986)
 :void (owner-id int) (transform transform-3d))

(defgmethod
 (collision-object-3d+shape-owner-get-transform :class 'collision-object-3d
  :bind "shape_owner_get_transform" :hash 1965739696)
 transform-3d (owner-id int))

(defgmethod
 (collision-object-3d+shape-owner-get-owner :class 'collision-object-3d :bind
  "shape_owner_get_owner" :hash 3332903315)
 object (owner-id int))

(defgmethod
 (collision-object-3d+shape-owner-set-disabled :class 'collision-object-3d
  :bind "shape_owner_set_disabled" :hash 300928843)
 :void (owner-id int) (disabled bool))

(defgmethod
 (collision-object-3d+is-shape-owner-disabled :class 'collision-object-3d :bind
  "is_shape_owner_disabled" :hash 1116898809)
 bool (owner-id int))

(defgmethod
 (collision-object-3d+shape-owner-add-shape :class 'collision-object-3d :bind
  "shape_owner_add_shape" :hash 2566676345)
 :void (owner-id int) (shape shape-3d))

(defgmethod
 (collision-object-3d+shape-owner-get-shape-count :class 'collision-object-3d
  :bind "shape_owner_get_shape_count" :hash 923996154)
 int (owner-id int))

(defgmethod
 (collision-object-3d+shape-owner-get-shape :class 'collision-object-3d :bind
  "shape_owner_get_shape" :hash 4015519174)
 shape-3d (owner-id int) (shape-id int))

(defgmethod
 (collision-object-3d+shape-owner-get-shape-index :class 'collision-object-3d
  :bind "shape_owner_get_shape_index" :hash 3175239445)
 int (owner-id int) (shape-id int))

(defgmethod
 (collision-object-3d+shape-owner-remove-shape :class 'collision-object-3d
  :bind "shape_owner_remove_shape" :hash 3937882851)
 :void (owner-id int) (shape-id int))

(defgmethod
 (collision-object-3d+shape-owner-clear-shapes :class 'collision-object-3d
  :bind "shape_owner_clear_shapes" :hash 1286410249)
 :void (owner-id int))

(defgmethod
 (collision-object-3d+shape-find-owner :class 'collision-object-3d :bind
  "shape_find_owner" :hash 923996154)
 int (shape-index int))