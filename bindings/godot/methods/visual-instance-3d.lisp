(common-lisp:in-package :%godot)


(defgmethod
 (visual-instance-3d+-get-aabb :class 'visual-instance-3d :bind "_get_aabb"
  :hash 1068685055 :virtual common-lisp:t)
 aabb)

(defgmethod
 (visual-instance-3d+set-base :class 'visual-instance-3d :bind "set_base" :hash
  2722037293)
 :void (base rid))

(defgmethod
 (visual-instance-3d+get-base :class 'visual-instance-3d :bind "get_base" :hash
  2944877500)
 rid)

(defgmethod
 (visual-instance-3d+get-instance :class 'visual-instance-3d :bind
  "get_instance" :hash 2944877500)
 rid)

(defgmethod
 (visual-instance-3d+set-layer-mask :class 'visual-instance-3d :bind
  "set_layer_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (visual-instance-3d+get-layer-mask :class 'visual-instance-3d :bind
  "get_layer_mask" :hash 3905245786)
 int)

(defgmethod
 (visual-instance-3d+set-layer-mask-value :class 'visual-instance-3d :bind
  "set_layer_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (visual-instance-3d+get-layer-mask-value :class 'visual-instance-3d :bind
  "get_layer_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (visual-instance-3d+set-sorting-offset :class 'visual-instance-3d :bind
  "set_sorting_offset" :hash 373806689)
 :void (offset float))

(defgmethod
 (visual-instance-3d+get-sorting-offset :class 'visual-instance-3d :bind
  "get_sorting_offset" :hash 1740695150)
 float)

(defgmethod
 (visual-instance-3d+set-sorting-use-aabb-center :class 'visual-instance-3d
  :bind "set_sorting_use_aabb_center" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (visual-instance-3d+is-sorting-use-aabb-center :class 'visual-instance-3d
  :bind "is_sorting_use_aabb_center" :hash 36873697)
 bool)

(defgmethod
 (visual-instance-3d+get-aabb :class 'visual-instance-3d :bind "get_aabb" :hash
  1068685055)
 aabb)