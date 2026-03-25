(common-lisp:in-package :%godot)


(defgmethod
 (sprite-2d+set-texture :class 'sprite-2d :bind "set_texture" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (sprite-2d+get-texture :class 'sprite-2d :bind "get_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (sprite-2d+set-centered :class 'sprite-2d :bind "set_centered" :hash
  2586408642)
 :void (centered bool))

(defgmethod
 (sprite-2d+is-centered :class 'sprite-2d :bind "is_centered" :hash 36873697)
 bool)

(defgmethod
 (sprite-2d+set-offset :class 'sprite-2d :bind "set_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (sprite-2d+get-offset :class 'sprite-2d :bind "get_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (sprite-2d+set-flip-h :class 'sprite-2d :bind "set_flip_h" :hash 2586408642)
 :void (flip-h bool))

(defgmethod
 (sprite-2d+is-flipped-h :class 'sprite-2d :bind "is_flipped_h" :hash 36873697)
 bool)

(defgmethod
 (sprite-2d+set-flip-v :class 'sprite-2d :bind "set_flip_v" :hash 2586408642)
 :void (flip-v bool))

(defgmethod
 (sprite-2d+is-flipped-v :class 'sprite-2d :bind "is_flipped_v" :hash 36873697)
 bool)

(defgmethod
 (sprite-2d+set-region-enabled :class 'sprite-2d :bind "set_region_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (sprite-2d+is-region-enabled :class 'sprite-2d :bind "is_region_enabled" :hash
  36873697)
 bool)

(defgmethod
 (sprite-2d+is-pixel-opaque :class 'sprite-2d :bind "is_pixel_opaque" :hash
  556197845)
 bool (pos vector-2))

(defgmethod
 (sprite-2d+set-region-rect :class 'sprite-2d :bind "set_region_rect" :hash
  2046264180)
 :void (rect rect-2))

(defgmethod
 (sprite-2d+get-region-rect :class 'sprite-2d :bind "get_region_rect" :hash
  1639390495)
 rect-2)

(defgmethod
 (sprite-2d+set-region-filter-clip-enabled :class 'sprite-2d :bind
  "set_region_filter_clip_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (sprite-2d+is-region-filter-clip-enabled :class 'sprite-2d :bind
  "is_region_filter_clip_enabled" :hash 36873697)
 bool)

(defgmethod
 (sprite-2d+set-frame :class 'sprite-2d :bind "set_frame" :hash 1286410249)
 :void (frame int))

(defgmethod
 (sprite-2d+get-frame :class 'sprite-2d :bind "get_frame" :hash 3905245786) int)

(defgmethod
 (sprite-2d+set-frame-coords :class 'sprite-2d :bind "set_frame_coords" :hash
  1130785943)
 :void (coords vector-2i))

(defgmethod
 (sprite-2d+get-frame-coords :class 'sprite-2d :bind "get_frame_coords" :hash
  3690982128)
 vector-2i)

(defgmethod
 (sprite-2d+set-vframes :class 'sprite-2d :bind "set_vframes" :hash 1286410249)
 :void (vframes int))

(defgmethod
 (sprite-2d+get-vframes :class 'sprite-2d :bind "get_vframes" :hash 3905245786)
 int)

(defgmethod
 (sprite-2d+set-hframes :class 'sprite-2d :bind "set_hframes" :hash 1286410249)
 :void (hframes int))

(defgmethod
 (sprite-2d+get-hframes :class 'sprite-2d :bind "get_hframes" :hash 3905245786)
 int)

(defgmethod
 (sprite-2d+get-rect :class 'sprite-2d :bind "get_rect" :hash 1639390495)
 rect-2)