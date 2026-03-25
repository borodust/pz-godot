(common-lisp:in-package :%godot)


(defgmethod
 (label-3d+set-horizontal-alignment :class 'label-3d :bind
  "set_horizontal_alignment" :hash 2312603777)
 :void (alignment horizontal-alignment))

(defgmethod
 (label-3d+get-horizontal-alignment :class 'label-3d :bind
  "get_horizontal_alignment" :hash 341400642)
 horizontal-alignment)

(defgmethod
 (label-3d+set-vertical-alignment :class 'label-3d :bind
  "set_vertical_alignment" :hash 1796458609)
 :void (alignment vertical-alignment))

(defgmethod
 (label-3d+get-vertical-alignment :class 'label-3d :bind
  "get_vertical_alignment" :hash 3274884059)
 vertical-alignment)

(defgmethod
 (label-3d+set-modulate :class 'label-3d :bind "set_modulate" :hash 2920490490)
 :void (modulate color))

(defgmethod
 (label-3d+get-modulate :class 'label-3d :bind "get_modulate" :hash 3444240500)
 color)

(defgmethod
 (label-3d+set-outline-modulate :class 'label-3d :bind "set_outline_modulate"
  :hash 2920490490)
 :void (modulate color))

(defgmethod
 (label-3d+get-outline-modulate :class 'label-3d :bind "get_outline_modulate"
  :hash 3444240500)
 color)

(defgmethod
 (label-3d+set-text :class 'label-3d :bind "set_text" :hash 83702148) :void
 (text string))

(defgmethod
 (label-3d+get-text :class 'label-3d :bind "get_text" :hash 201670096) string)

(defgmethod
 (label-3d+set-text-direction :class 'label-3d :bind "set_text_direction" :hash
  1418190634)
 :void (direction text-server+direction))

(defgmethod
 (label-3d+get-text-direction :class 'label-3d :bind "get_text_direction" :hash
  2516697328)
 text-server+direction)

(defgmethod
 (label-3d+set-language :class 'label-3d :bind "set_language" :hash 83702148)
 :void (language string))

(defgmethod
 (label-3d+get-language :class 'label-3d :bind "get_language" :hash 201670096)
 string)

(defgmethod
 (label-3d+set-structured-text-bidi-override :class 'label-3d :bind
  "set_structured_text_bidi_override" :hash 55961453)
 :void (parser text-server+structured-text-parser))

(defgmethod
 (label-3d+get-structured-text-bidi-override :class 'label-3d :bind
  "get_structured_text_bidi_override" :hash 3385126229)
 text-server+structured-text-parser)

(defgmethod
 (label-3d+set-structured-text-bidi-override-options :class 'label-3d :bind
  "set_structured_text_bidi_override_options" :hash 381264803)
 :void (args array))

(defgmethod
 (label-3d+get-structured-text-bidi-override-options :class 'label-3d :bind
  "get_structured_text_bidi_override_options" :hash 3995934104)
 array)

(defgmethod
 (label-3d+set-uppercase :class 'label-3d :bind "set_uppercase" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (label-3d+is-uppercase :class 'label-3d :bind "is_uppercase" :hash 36873697)
 bool)

(defgmethod
 (label-3d+set-render-priority :class 'label-3d :bind "set_render_priority"
  :hash 1286410249)
 :void (priority int))

(defgmethod
 (label-3d+get-render-priority :class 'label-3d :bind "get_render_priority"
  :hash 3905245786)
 int)

(defgmethod
 (label-3d+set-outline-render-priority :class 'label-3d :bind
  "set_outline_render_priority" :hash 1286410249)
 :void (priority int))

(defgmethod
 (label-3d+get-outline-render-priority :class 'label-3d :bind
  "get_outline_render_priority" :hash 3905245786)
 int)

(defgmethod
 (label-3d+set-font :class 'label-3d :bind "set_font" :hash 1262170328) :void
 (font font))

(defgmethod
 (label-3d+get-font :class 'label-3d :bind "get_font" :hash 3229501585) font)

(defgmethod
 (label-3d+set-font-size :class 'label-3d :bind "set_font_size" :hash
  1286410249)
 :void (size int))

(defgmethod
 (label-3d+get-font-size :class 'label-3d :bind "get_font_size" :hash
  3905245786)
 int)

(defgmethod
 (label-3d+set-outline-size :class 'label-3d :bind "set_outline_size" :hash
  1286410249)
 :void (outline-size int))

(defgmethod
 (label-3d+get-outline-size :class 'label-3d :bind "get_outline_size" :hash
  3905245786)
 int)

(defgmethod
 (label-3d+set-line-spacing :class 'label-3d :bind "set_line_spacing" :hash
  373806689)
 :void (line-spacing float))

(defgmethod
 (label-3d+get-line-spacing :class 'label-3d :bind "get_line_spacing" :hash
  1740695150)
 float)

(defgmethod
 (label-3d+set-autowrap-mode :class 'label-3d :bind "set_autowrap_mode" :hash
  3289138044)
 :void (autowrap-mode text-server+autowrap-mode))

(defgmethod
 (label-3d+get-autowrap-mode :class 'label-3d :bind "get_autowrap_mode" :hash
  1549071663)
 text-server+autowrap-mode)

(defgmethod
 (label-3d+set-autowrap-trim-flags :class 'label-3d :bind
  "set_autowrap_trim_flags" :hash 2809697122)
 :void (autowrap-trim-flags text-server+line-break-flag))

(defgmethod
 (label-3d+get-autowrap-trim-flags :class 'label-3d :bind
  "get_autowrap_trim_flags" :hash 2340632602)
 text-server+line-break-flag)

(defgmethod
 (label-3d+set-justification-flags :class 'label-3d :bind
  "set_justification_flags" :hash 2877345813)
 :void (justification-flags text-server+justification-flag))

(defgmethod
 (label-3d+get-justification-flags :class 'label-3d :bind
  "get_justification_flags" :hash 1583363614)
 text-server+justification-flag)

(defgmethod
 (label-3d+set-width :class 'label-3d :bind "set_width" :hash 373806689) :void
 (width float))

(defgmethod
 (label-3d+get-width :class 'label-3d :bind "get_width" :hash 1740695150) float)

(defgmethod
 (label-3d+set-pixel-size :class 'label-3d :bind "set_pixel_size" :hash
  373806689)
 :void (pixel-size float))

(defgmethod
 (label-3d+get-pixel-size :class 'label-3d :bind "get_pixel_size" :hash
  1740695150)
 float)

(defgmethod
 (label-3d+set-offset :class 'label-3d :bind "set_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (label-3d+get-offset :class 'label-3d :bind "get_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (label-3d+set-draw-flag :class 'label-3d :bind "set_draw_flag" :hash
  1285833066)
 :void (flag label-3d+draw-flags) (enabled bool))

(defgmethod
 (label-3d+get-draw-flag :class 'label-3d :bind "get_draw_flag" :hash
  259226453)
 bool (flag label-3d+draw-flags))

(defgmethod
 (label-3d+set-billboard-mode :class 'label-3d :bind "set_billboard_mode" :hash
  4202036497)
 :void (mode base-material-3d+billboard-mode))

(defgmethod
 (label-3d+get-billboard-mode :class 'label-3d :bind "get_billboard_mode" :hash
  1283840139)
 base-material-3d+billboard-mode)

(defgmethod
 (label-3d+set-alpha-cut-mode :class 'label-3d :bind "set_alpha_cut_mode" :hash
  2549142916)
 :void (mode label-3d+alpha-cut-mode))

(defgmethod
 (label-3d+get-alpha-cut-mode :class 'label-3d :bind "get_alpha_cut_mode" :hash
  219468601)
 label-3d+alpha-cut-mode)

(defgmethod
 (label-3d+set-alpha-scissor-threshold :class 'label-3d :bind
  "set_alpha_scissor_threshold" :hash 373806689)
 :void (threshold float))

(defgmethod
 (label-3d+get-alpha-scissor-threshold :class 'label-3d :bind
  "get_alpha_scissor_threshold" :hash 1740695150)
 float)

(defgmethod
 (label-3d+set-alpha-hash-scale :class 'label-3d :bind "set_alpha_hash_scale"
  :hash 373806689)
 :void (threshold float))

(defgmethod
 (label-3d+get-alpha-hash-scale :class 'label-3d :bind "get_alpha_hash_scale"
  :hash 1740695150)
 float)

(defgmethod
 (label-3d+set-alpha-antialiasing :class 'label-3d :bind
  "set_alpha_antialiasing" :hash 3212649852)
 :void (alpha-aa base-material-3d+alpha-anti-aliasing))

(defgmethod
 (label-3d+get-alpha-antialiasing :class 'label-3d :bind
  "get_alpha_antialiasing" :hash 2889939400)
 base-material-3d+alpha-anti-aliasing)

(defgmethod
 (label-3d+set-alpha-antialiasing-edge :class 'label-3d :bind
  "set_alpha_antialiasing_edge" :hash 373806689)
 :void (edge float))

(defgmethod
 (label-3d+get-alpha-antialiasing-edge :class 'label-3d :bind
  "get_alpha_antialiasing_edge" :hash 1740695150)
 float)

(defgmethod
 (label-3d+set-texture-filter :class 'label-3d :bind "set_texture_filter" :hash
  22904437)
 :void (mode base-material-3d+texture-filter))

(defgmethod
 (label-3d+get-texture-filter :class 'label-3d :bind "get_texture_filter" :hash
  3289213076)
 base-material-3d+texture-filter)

(defgmethod
 (label-3d+generate-triangle-mesh :class 'label-3d :bind
  "generate_triangle_mesh" :hash 3476533166)
 triangle-mesh)