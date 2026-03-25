(common-lisp:in-package :%godot)


(defgmethod
 (sprite-3d+set-texture :class 'sprite-3d :bind "set_texture" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (sprite-3d+get-texture :class 'sprite-3d :bind "get_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (sprite-3d+set-region-enabled :class 'sprite-3d :bind "set_region_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (sprite-3d+is-region-enabled :class 'sprite-3d :bind "is_region_enabled" :hash
  36873697)
 bool)

(defgmethod
 (sprite-3d+set-region-rect :class 'sprite-3d :bind "set_region_rect" :hash
  2046264180)
 :void (rect rect-2))

(defgmethod
 (sprite-3d+get-region-rect :class 'sprite-3d :bind "get_region_rect" :hash
  1639390495)
 rect-2)

(defgmethod
 (sprite-3d+set-frame :class 'sprite-3d :bind "set_frame" :hash 1286410249)
 :void (frame int))

(defgmethod
 (sprite-3d+get-frame :class 'sprite-3d :bind "get_frame" :hash 3905245786) int)

(defgmethod
 (sprite-3d+set-frame-coords :class 'sprite-3d :bind "set_frame_coords" :hash
  1130785943)
 :void (coords vector-2i))

(defgmethod
 (sprite-3d+get-frame-coords :class 'sprite-3d :bind "get_frame_coords" :hash
  3690982128)
 vector-2i)

(defgmethod
 (sprite-3d+set-vframes :class 'sprite-3d :bind "set_vframes" :hash 1286410249)
 :void (vframes int))

(defgmethod
 (sprite-3d+get-vframes :class 'sprite-3d :bind "get_vframes" :hash 3905245786)
 int)

(defgmethod
 (sprite-3d+set-hframes :class 'sprite-3d :bind "set_hframes" :hash 1286410249)
 :void (hframes int))

(defgmethod
 (sprite-3d+get-hframes :class 'sprite-3d :bind "get_hframes" :hash 3905245786)
 int)