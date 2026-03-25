(common-lisp:in-package :%godot)


(defgmethod
 (sprite-base-3d+set-centered :class 'sprite-base-3d :bind "set_centered" :hash
  2586408642)
 :void (centered bool))

(defgmethod
 (sprite-base-3d+is-centered :class 'sprite-base-3d :bind "is_centered" :hash
  36873697)
 bool)

(defgmethod
 (sprite-base-3d+set-offset :class 'sprite-base-3d :bind "set_offset" :hash
  743155724)
 :void (offset vector-2))

(defgmethod
 (sprite-base-3d+get-offset :class 'sprite-base-3d :bind "get_offset" :hash
  3341600327)
 vector-2)

(defgmethod
 (sprite-base-3d+set-flip-h :class 'sprite-base-3d :bind "set_flip_h" :hash
  2586408642)
 :void (flip-h bool))

(defgmethod
 (sprite-base-3d+is-flipped-h :class 'sprite-base-3d :bind "is_flipped_h" :hash
  36873697)
 bool)

(defgmethod
 (sprite-base-3d+set-flip-v :class 'sprite-base-3d :bind "set_flip_v" :hash
  2586408642)
 :void (flip-v bool))

(defgmethod
 (sprite-base-3d+is-flipped-v :class 'sprite-base-3d :bind "is_flipped_v" :hash
  36873697)
 bool)

(defgmethod
 (sprite-base-3d+set-modulate :class 'sprite-base-3d :bind "set_modulate" :hash
  2920490490)
 :void (modulate color))

(defgmethod
 (sprite-base-3d+get-modulate :class 'sprite-base-3d :bind "get_modulate" :hash
  3444240500)
 color)

(defgmethod
 (sprite-base-3d+set-render-priority :class 'sprite-base-3d :bind
  "set_render_priority" :hash 1286410249)
 :void (priority int))

(defgmethod
 (sprite-base-3d+get-render-priority :class 'sprite-base-3d :bind
  "get_render_priority" :hash 3905245786)
 int)

(defgmethod
 (sprite-base-3d+set-pixel-size :class 'sprite-base-3d :bind "set_pixel_size"
  :hash 373806689)
 :void (pixel-size float))

(defgmethod
 (sprite-base-3d+get-pixel-size :class 'sprite-base-3d :bind "get_pixel_size"
  :hash 1740695150)
 float)

(defgmethod
 (sprite-base-3d+set-axis :class 'sprite-base-3d :bind "set_axis" :hash
  1144690656)
 :void (axis vector-3+axis))

(defgmethod
 (sprite-base-3d+get-axis :class 'sprite-base-3d :bind "get_axis" :hash
  3050976882)
 vector-3+axis)

(defgmethod
 (sprite-base-3d+set-draw-flag :class 'sprite-base-3d :bind "set_draw_flag"
  :hash 1135633219)
 :void (flag sprite-base-3d+draw-flags) (enabled bool))

(defgmethod
 (sprite-base-3d+get-draw-flag :class 'sprite-base-3d :bind "get_draw_flag"
  :hash 1733036628)
 bool (flag sprite-base-3d+draw-flags))

(defgmethod
 (sprite-base-3d+set-alpha-cut-mode :class 'sprite-base-3d :bind
  "set_alpha_cut_mode" :hash 227561226)
 :void (mode sprite-base-3d+alpha-cut-mode))

(defgmethod
 (sprite-base-3d+get-alpha-cut-mode :class 'sprite-base-3d :bind
  "get_alpha_cut_mode" :hash 336003791)
 sprite-base-3d+alpha-cut-mode)

(defgmethod
 (sprite-base-3d+set-alpha-scissor-threshold :class 'sprite-base-3d :bind
  "set_alpha_scissor_threshold" :hash 373806689)
 :void (threshold float))

(defgmethod
 (sprite-base-3d+get-alpha-scissor-threshold :class 'sprite-base-3d :bind
  "get_alpha_scissor_threshold" :hash 1740695150)
 float)

(defgmethod
 (sprite-base-3d+set-alpha-hash-scale :class 'sprite-base-3d :bind
  "set_alpha_hash_scale" :hash 373806689)
 :void (threshold float))

(defgmethod
 (sprite-base-3d+get-alpha-hash-scale :class 'sprite-base-3d :bind
  "get_alpha_hash_scale" :hash 1740695150)
 float)

(defgmethod
 (sprite-base-3d+set-alpha-antialiasing :class 'sprite-base-3d :bind
  "set_alpha_antialiasing" :hash 3212649852)
 :void (alpha-aa base-material-3d+alpha-anti-aliasing))

(defgmethod
 (sprite-base-3d+get-alpha-antialiasing :class 'sprite-base-3d :bind
  "get_alpha_antialiasing" :hash 2889939400)
 base-material-3d+alpha-anti-aliasing)

(defgmethod
 (sprite-base-3d+set-alpha-antialiasing-edge :class 'sprite-base-3d :bind
  "set_alpha_antialiasing_edge" :hash 373806689)
 :void (edge float))

(defgmethod
 (sprite-base-3d+get-alpha-antialiasing-edge :class 'sprite-base-3d :bind
  "get_alpha_antialiasing_edge" :hash 1740695150)
 float)

(defgmethod
 (sprite-base-3d+set-billboard-mode :class 'sprite-base-3d :bind
  "set_billboard_mode" :hash 4202036497)
 :void (mode base-material-3d+billboard-mode))

(defgmethod
 (sprite-base-3d+get-billboard-mode :class 'sprite-base-3d :bind
  "get_billboard_mode" :hash 1283840139)
 base-material-3d+billboard-mode)

(defgmethod
 (sprite-base-3d+set-texture-filter :class 'sprite-base-3d :bind
  "set_texture_filter" :hash 22904437)
 :void (mode base-material-3d+texture-filter))

(defgmethod
 (sprite-base-3d+get-texture-filter :class 'sprite-base-3d :bind
  "get_texture_filter" :hash 3289213076)
 base-material-3d+texture-filter)

(defgmethod
 (sprite-base-3d+get-item-rect :class 'sprite-base-3d :bind "get_item_rect"
  :hash 1639390495)
 rect-2)

(defgmethod
 (sprite-base-3d+generate-triangle-mesh :class 'sprite-base-3d :bind
  "generate_triangle_mesh" :hash 3476533166)
 triangle-mesh)