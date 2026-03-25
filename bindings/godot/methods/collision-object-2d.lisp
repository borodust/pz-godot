(common-lisp:in-package :%godot)


(defgmethod
 (collision-object-2d+-input-event :class 'collision-object-2d :bind
  "_input_event" :hash 1847696837 :virtual common-lisp:t)
 :void (viewport viewport) (event input-event) (shape-idx int))

(defgmethod
 (collision-object-2d+-mouse-enter :class 'collision-object-2d :bind
  "_mouse_enter" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (collision-object-2d+-mouse-exit :class 'collision-object-2d :bind
  "_mouse_exit" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (collision-object-2d+-mouse-shape-enter :class 'collision-object-2d :bind
  "_mouse_shape_enter" :hash 1286410249 :virtual common-lisp:t)
 :void (shape-idx int))

(defgmethod
 (collision-object-2d+-mouse-shape-exit :class 'collision-object-2d :bind
  "_mouse_shape_exit" :hash 1286410249 :virtual common-lisp:t)
 :void (shape-idx int))

(defgmethod
 (collision-object-2d+get-rid :class 'collision-object-2d :bind "get_rid" :hash
  2944877500)
 rid)

(defgmethod
 (collision-object-2d+set-collision-layer :class 'collision-object-2d :bind
  "set_collision_layer" :hash 1286410249)
 :void (layer int))

(defgmethod
 (collision-object-2d+get-collision-layer :class 'collision-object-2d :bind
  "get_collision_layer" :hash 3905245786)
 int)

(defgmethod
 (collision-object-2d+set-collision-mask :class 'collision-object-2d :bind
  "set_collision_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (collision-object-2d+get-collision-mask :class 'collision-object-2d :bind
  "get_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (collision-object-2d+set-collision-layer-value :class 'collision-object-2d
  :bind "set_collision_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (collision-object-2d+get-collision-layer-value :class 'collision-object-2d
  :bind "get_collision_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (collision-object-2d+set-collision-mask-value :class 'collision-object-2d
  :bind "set_collision_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (collision-object-2d+get-collision-mask-value :class 'collision-object-2d
  :bind "get_collision_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (collision-object-2d+set-collision-priority :class 'collision-object-2d :bind
  "set_collision_priority" :hash 373806689)
 :void (priority float))

(defgmethod
 (collision-object-2d+get-collision-priority :class 'collision-object-2d :bind
  "get_collision_priority" :hash 1740695150)
 float)

(defgmethod
 (collision-object-2d+set-disable-mode :class 'collision-object-2d :bind
  "set_disable_mode" :hash 1919204045)
 :void (mode collision-object-2d+disable-mode))

(defgmethod
 (collision-object-2d+get-disable-mode :class 'collision-object-2d :bind
  "get_disable_mode" :hash 3172846349)
 collision-object-2d+disable-mode)

(defgmethod
 (collision-object-2d+set-pickable :class 'collision-object-2d :bind
  "set_pickable" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (collision-object-2d+is-pickable :class 'collision-object-2d :bind
  "is_pickable" :hash 36873697)
 bool)

(defgmethod
 (collision-object-2d+create-shape-owner :class 'collision-object-2d :bind
  "create_shape_owner" :hash 3429307534)
 int (owner object))

(defgmethod
 (collision-object-2d+remove-shape-owner :class 'collision-object-2d :bind
  "remove_shape_owner" :hash 1286410249)
 :void (owner-id int))

(defgmethod
 (collision-object-2d+get-shape-owners :class 'collision-object-2d :bind
  "get_shape_owners" :hash 969006518)
 packed-int-32array)

(defgmethod
 (collision-object-2d+shape-owner-set-transform :class 'collision-object-2d
  :bind "shape_owner_set_transform" :hash 30160968)
 :void (owner-id int) (transform transform-2d))

(defgmethod
 (collision-object-2d+shape-owner-get-transform :class 'collision-object-2d
  :bind "shape_owner_get_transform" :hash 3836996910)
 transform-2d (owner-id int))

(defgmethod
 (collision-object-2d+shape-owner-get-owner :class 'collision-object-2d :bind
  "shape_owner_get_owner" :hash 3332903315)
 object (owner-id int))

(defgmethod
 (collision-object-2d+shape-owner-set-disabled :class 'collision-object-2d
  :bind "shape_owner_set_disabled" :hash 300928843)
 :void (owner-id int) (disabled bool))

(defgmethod
 (collision-object-2d+is-shape-owner-disabled :class 'collision-object-2d :bind
  "is_shape_owner_disabled" :hash 1116898809)
 bool (owner-id int))

(defgmethod
 (collision-object-2d+shape-owner-set-one-way-collision :class
  'collision-object-2d :bind "shape_owner_set_one_way_collision" :hash
  300928843)
 :void (owner-id int) (enable bool))

(defgmethod
 (collision-object-2d+is-shape-owner-one-way-collision-enabled :class
  'collision-object-2d :bind "is_shape_owner_one_way_collision_enabled" :hash
  1116898809)
 bool (owner-id int))

(defgmethod
 (collision-object-2d+shape-owner-set-one-way-collision-margin :class
  'collision-object-2d :bind "shape_owner_set_one_way_collision_margin" :hash
  1602489585)
 :void (owner-id int) (margin float))

(defgmethod
 (collision-object-2d+get-shape-owner-one-way-collision-margin :class
  'collision-object-2d :bind "get_shape_owner_one_way_collision_margin" :hash
  2339986948)
 float (owner-id int))

(defgmethod
 (collision-object-2d+shape-owner-add-shape :class 'collision-object-2d :bind
  "shape_owner_add_shape" :hash 2077425081)
 :void (owner-id int) (shape shape-2d))

(defgmethod
 (collision-object-2d+shape-owner-get-shape-count :class 'collision-object-2d
  :bind "shape_owner_get_shape_count" :hash 923996154)
 int (owner-id int))

(defgmethod
 (collision-object-2d+shape-owner-get-shape :class 'collision-object-2d :bind
  "shape_owner_get_shape" :hash 3106725749)
 shape-2d (owner-id int) (shape-id int))

(defgmethod
 (collision-object-2d+shape-owner-get-shape-index :class 'collision-object-2d
  :bind "shape_owner_get_shape_index" :hash 3175239445)
 int (owner-id int) (shape-id int))

(defgmethod
 (collision-object-2d+shape-owner-remove-shape :class 'collision-object-2d
  :bind "shape_owner_remove_shape" :hash 3937882851)
 :void (owner-id int) (shape-id int))

(defgmethod
 (collision-object-2d+shape-owner-clear-shapes :class 'collision-object-2d
  :bind "shape_owner_clear_shapes" :hash 1286410249)
 :void (owner-id int))

(defgmethod
 (collision-object-2d+shape-find-owner :class 'collision-object-2d :bind
  "shape_find_owner" :hash 923996154)
 int (shape-index int))