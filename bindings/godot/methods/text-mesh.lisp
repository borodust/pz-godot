(common-lisp:in-package :%godot)


(defgmethod
 (text-mesh+set-horizontal-alignment :class 'text-mesh :bind
  "set_horizontal_alignment" :hash 2312603777)
 :void (alignment horizontal-alignment))

(defgmethod
 (text-mesh+get-horizontal-alignment :class 'text-mesh :bind
  "get_horizontal_alignment" :hash 341400642)
 horizontal-alignment)

(defgmethod
 (text-mesh+set-vertical-alignment :class 'text-mesh :bind
  "set_vertical_alignment" :hash 1796458609)
 :void (alignment vertical-alignment))

(defgmethod
 (text-mesh+get-vertical-alignment :class 'text-mesh :bind
  "get_vertical_alignment" :hash 3274884059)
 vertical-alignment)

(defgmethod
 (text-mesh+set-text :class 'text-mesh :bind "set_text" :hash 83702148) :void
 (text string))

(defgmethod
 (text-mesh+get-text :class 'text-mesh :bind "get_text" :hash 201670096) string)

(defgmethod
 (text-mesh+set-font :class 'text-mesh :bind "set_font" :hash 1262170328) :void
 (font font))

(defgmethod
 (text-mesh+get-font :class 'text-mesh :bind "get_font" :hash 3229501585) font)

(defgmethod
 (text-mesh+set-font-size :class 'text-mesh :bind "set_font_size" :hash
  1286410249)
 :void (font-size int))

(defgmethod
 (text-mesh+get-font-size :class 'text-mesh :bind "get_font_size" :hash
  3905245786)
 int)

(defgmethod
 (text-mesh+set-line-spacing :class 'text-mesh :bind "set_line_spacing" :hash
  373806689)
 :void (line-spacing float))

(defgmethod
 (text-mesh+get-line-spacing :class 'text-mesh :bind "get_line_spacing" :hash
  1740695150)
 float)

(defgmethod
 (text-mesh+set-autowrap-mode :class 'text-mesh :bind "set_autowrap_mode" :hash
  3289138044)
 :void (autowrap-mode text-server+autowrap-mode))

(defgmethod
 (text-mesh+get-autowrap-mode :class 'text-mesh :bind "get_autowrap_mode" :hash
  1549071663)
 text-server+autowrap-mode)

(defgmethod
 (text-mesh+set-justification-flags :class 'text-mesh :bind
  "set_justification_flags" :hash 2877345813)
 :void (justification-flags text-server+justification-flag))

(defgmethod
 (text-mesh+get-justification-flags :class 'text-mesh :bind
  "get_justification_flags" :hash 1583363614)
 text-server+justification-flag)

(defgmethod
 (text-mesh+set-depth :class 'text-mesh :bind "set_depth" :hash 373806689)
 :void (depth float))

(defgmethod
 (text-mesh+get-depth :class 'text-mesh :bind "get_depth" :hash 1740695150)
 float)

(defgmethod
 (text-mesh+set-width :class 'text-mesh :bind "set_width" :hash 373806689)
 :void (width float))

(defgmethod
 (text-mesh+get-width :class 'text-mesh :bind "get_width" :hash 1740695150)
 float)

(defgmethod
 (text-mesh+set-pixel-size :class 'text-mesh :bind "set_pixel_size" :hash
  373806689)
 :void (pixel-size float))

(defgmethod
 (text-mesh+get-pixel-size :class 'text-mesh :bind "get_pixel_size" :hash
  1740695150)
 float)

(defgmethod
 (text-mesh+set-offset :class 'text-mesh :bind "set_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (text-mesh+get-offset :class 'text-mesh :bind "get_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (text-mesh+set-curve-step :class 'text-mesh :bind "set_curve_step" :hash
  373806689)
 :void (curve-step float))

(defgmethod
 (text-mesh+get-curve-step :class 'text-mesh :bind "get_curve_step" :hash
  1740695150)
 float)

(defgmethod
 (text-mesh+set-text-direction :class 'text-mesh :bind "set_text_direction"
  :hash 1418190634)
 :void (direction text-server+direction))

(defgmethod
 (text-mesh+get-text-direction :class 'text-mesh :bind "get_text_direction"
  :hash 2516697328)
 text-server+direction)

(defgmethod
 (text-mesh+set-language :class 'text-mesh :bind "set_language" :hash 83702148)
 :void (language string))

(defgmethod
 (text-mesh+get-language :class 'text-mesh :bind "get_language" :hash
  201670096)
 string)

(defgmethod
 (text-mesh+set-structured-text-bidi-override :class 'text-mesh :bind
  "set_structured_text_bidi_override" :hash 55961453)
 :void (parser text-server+structured-text-parser))

(defgmethod
 (text-mesh+get-structured-text-bidi-override :class 'text-mesh :bind
  "get_structured_text_bidi_override" :hash 3385126229)
 text-server+structured-text-parser)

(defgmethod
 (text-mesh+set-structured-text-bidi-override-options :class 'text-mesh :bind
  "set_structured_text_bidi_override_options" :hash 381264803)
 :void (args array))

(defgmethod
 (text-mesh+get-structured-text-bidi-override-options :class 'text-mesh :bind
  "get_structured_text_bidi_override_options" :hash 3995934104)
 array)

(defgmethod
 (text-mesh+set-uppercase :class 'text-mesh :bind "set_uppercase" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (text-mesh+is-uppercase :class 'text-mesh :bind "is_uppercase" :hash 36873697)
 bool)