(common-lisp:in-package :%godot)


(defgmethod
 (texture-2d+%get-image :class 'texture-2d :bind "_get_image" :hash 4190603485
  :virtual common-lisp:t)
 image)

(defgmethod
 (texture-2d+%get-format :class 'texture-2d :bind "_get_format" :hash
  3847873762 :virtual common-lisp:t)
 image+format)

(defgmethod
 (texture-2d+%get-mipmap-count :class 'texture-2d :bind "_get_mipmap_count"
  :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (texture-2d+%get-width :class 'texture-2d :bind "_get_width" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (texture-2d+%get-height :class 'texture-2d :bind "_get_height" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (texture-2d+%is-pixel-opaque :class 'texture-2d :bind "_is_pixel_opaque" :hash
  2522259332 :virtual common-lisp:t)
 bool (x int) (y int))

(defgmethod
 (texture-2d+%has-alpha :class 'texture-2d :bind "_has_alpha" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (texture-2d+%has-mipmaps :class 'texture-2d :bind "_has_mipmaps" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (texture-2d+%draw :class 'texture-2d :bind "_draw" :hash 1384643611 :virtual
  common-lisp:t)
 :void (to-canvas-item rid) (pos vector-2) (modulate color) (transpose bool))

(defgmethod
 (texture-2d+%draw-rect :class 'texture-2d :bind "_draw_rect" :hash 3819628907
  :virtual common-lisp:t)
 :void (to-canvas-item rid) (rect rect-2) (tile bool) (modulate color)
 (transpose bool))

(defgmethod
 (texture-2d+%draw-rect-region :class 'texture-2d :bind "_draw_rect_region"
  :hash 4094143664 :virtual common-lisp:t)
 :void (to-canvas-item rid) (rect rect-2) (src-rect rect-2) (modulate color)
 (transpose bool) (clip-uv bool))

(defgmethod
 (texture-2d+get-format :class 'texture-2d :bind "get_format" :hash 3847873762)
 image+format)

(defgmethod
 (texture-2d+get-mipmap-count :class 'texture-2d :bind "get_mipmap_count" :hash
  3905245786)
 int)

(defgmethod
 (texture-2d+get-width :class 'texture-2d :bind "get_width" :hash 3905245786)
 int)

(defgmethod
 (texture-2d+get-height :class 'texture-2d :bind "get_height" :hash 3905245786)
 int)

(defgmethod
 (texture-2d+get-size :class 'texture-2d :bind "get_size" :hash 3341600327)
 vector-2)

(defgmethod
 (texture-2d+has-alpha :class 'texture-2d :bind "has_alpha" :hash 36873697)
 bool)

(defgmethod
 (texture-2d+has-mipmaps :class 'texture-2d :bind "has_mipmaps" :hash 36873697)
 bool)

(defgmethod (texture-2d+draw :class 'texture-2d :bind "draw" :hash 2729649137)
 :void (canvas-item rid) (position vector-2) (modulate color) (transpose bool))

(defgmethod
 (texture-2d+draw-rect :class 'texture-2d :bind "draw_rect" :hash 3499451691)
 :void (canvas-item rid) (rect rect-2) (tile bool) (modulate color)
 (transpose bool))

(defgmethod
 (texture-2d+draw-rect-region :class 'texture-2d :bind "draw_rect_region" :hash
  2963678660)
 :void (canvas-item rid) (rect rect-2) (src-rect rect-2) (modulate color)
 (transpose bool) (clip-uv bool))

(defgmethod
 (texture-2d+get-image :class 'texture-2d :bind "get_image" :hash 4190603485)
 image)

(defgmethod
 (texture-2d+create-placeholder :class 'texture-2d :bind "create_placeholder"
  :hash 121922552)
 resource)