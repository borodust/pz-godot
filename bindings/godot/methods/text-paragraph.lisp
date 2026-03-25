(common-lisp:in-package :%godot)


(defgmethod
 (text-paragraph+clear :class 'text-paragraph :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (text-paragraph+duplicate :class 'text-paragraph :bind "duplicate" :hash
  3607706709)
 text-paragraph)

(defgmethod
 (text-paragraph+set-direction :class 'text-paragraph :bind "set_direction"
  :hash 1418190634)
 :void (direction text-server+direction))

(defgmethod
 (text-paragraph+get-direction :class 'text-paragraph :bind "get_direction"
  :hash 2516697328)
 text-server+direction)

(defgmethod
 (text-paragraph+get-inferred-direction :class 'text-paragraph :bind
  "get_inferred_direction" :hash 2516697328)
 text-server+direction)

(defgmethod
 (text-paragraph+set-custom-punctuation :class 'text-paragraph :bind
  "set_custom_punctuation" :hash 83702148)
 :void (custom-punctuation string))

(defgmethod
 (text-paragraph+get-custom-punctuation :class 'text-paragraph :bind
  "get_custom_punctuation" :hash 201670096)
 string)

(defgmethod
 (text-paragraph+set-orientation :class 'text-paragraph :bind "set_orientation"
  :hash 42823726)
 :void (orientation text-server+orientation))

(defgmethod
 (text-paragraph+get-orientation :class 'text-paragraph :bind "get_orientation"
  :hash 175768116)
 text-server+orientation)

(defgmethod
 (text-paragraph+set-preserve-invalid :class 'text-paragraph :bind
  "set_preserve_invalid" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-paragraph+get-preserve-invalid :class 'text-paragraph :bind
  "get_preserve_invalid" :hash 36873697)
 bool)

(defgmethod
 (text-paragraph+set-preserve-control :class 'text-paragraph :bind
  "set_preserve_control" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-paragraph+get-preserve-control :class 'text-paragraph :bind
  "get_preserve_control" :hash 36873697)
 bool)

(defgmethod
 (text-paragraph+set-bidi-override :class 'text-paragraph :bind
  "set_bidi_override" :hash 381264803)
 :void (override array))

(defgmethod
 (text-paragraph+set-dropcap :class 'text-paragraph :bind "set_dropcap" :hash
  2498990330)
 bool (text string) (font font) (font-size int) (dropcap-margins rect-2)
 (language string))

(defgmethod
 (text-paragraph+clear-dropcap :class 'text-paragraph :bind "clear_dropcap"
  :hash 3218959716)
 :void)

(defgmethod
 (text-paragraph+add-string :class 'text-paragraph :bind "add_string" :hash
  621426851)
 bool (text string) (font font) (font-size int) (language string)
 (meta variant))

(defgmethod
 (text-paragraph+add-object :class 'text-paragraph :bind "add_object" :hash
  1316529304)
 bool (key variant) (size vector-2) (inline-align inline-alignment)
 (length int) (baseline float))

(defgmethod
 (text-paragraph+resize-object :class 'text-paragraph :bind "resize_object"
  :hash 2095776372)
 bool (key variant) (size vector-2) (inline-align inline-alignment)
 (baseline float))

(defgmethod
 (text-paragraph+has-object :class 'text-paragraph :bind "has_object" :hash
  77467830)
 bool (key variant))

(defgmethod
 (text-paragraph+set-alignment :class 'text-paragraph :bind "set_alignment"
  :hash 2312603777)
 :void (alignment horizontal-alignment))

(defgmethod
 (text-paragraph+get-alignment :class 'text-paragraph :bind "get_alignment"
  :hash 341400642)
 horizontal-alignment)

(defgmethod
 (text-paragraph+tab-align :class 'text-paragraph :bind "tab_align" :hash
  2899603908)
 :void (tab-stops packed-float-32array))

(defgmethod
 (text-paragraph+set-break-flags :class 'text-paragraph :bind "set_break_flags"
  :hash 2809697122)
 :void (flags text-server+line-break-flag))

(defgmethod
 (text-paragraph+get-break-flags :class 'text-paragraph :bind "get_break_flags"
  :hash 2340632602)
 text-server+line-break-flag)

(defgmethod
 (text-paragraph+set-justification-flags :class 'text-paragraph :bind
  "set_justification_flags" :hash 2877345813)
 :void (flags text-server+justification-flag))

(defgmethod
 (text-paragraph+get-justification-flags :class 'text-paragraph :bind
  "get_justification_flags" :hash 1583363614)
 text-server+justification-flag)

(defgmethod
 (text-paragraph+set-text-overrun-behavior :class 'text-paragraph :bind
  "set_text_overrun_behavior" :hash 1008890932)
 :void (overrun-behavior text-server+overrun-behavior))

(defgmethod
 (text-paragraph+get-text-overrun-behavior :class 'text-paragraph :bind
  "get_text_overrun_behavior" :hash 3779142101)
 text-server+overrun-behavior)

(defgmethod
 (text-paragraph+set-ellipsis-char :class 'text-paragraph :bind
  "set_ellipsis_char" :hash 83702148)
 :void (char string))

(defgmethod
 (text-paragraph+get-ellipsis-char :class 'text-paragraph :bind
  "get_ellipsis_char" :hash 201670096)
 string)

(defgmethod
 (text-paragraph+set-width :class 'text-paragraph :bind "set_width" :hash
  373806689)
 :void (width float))

(defgmethod
 (text-paragraph+get-width :class 'text-paragraph :bind "get_width" :hash
  1740695150)
 float)

(defgmethod
 (text-paragraph+get-non-wrapped-size :class 'text-paragraph :bind
  "get_non_wrapped_size" :hash 3341600327)
 vector-2)

(defgmethod
 (text-paragraph+get-size :class 'text-paragraph :bind "get_size" :hash
  3341600327)
 vector-2)

(defgmethod
 (text-paragraph+get-rid :class 'text-paragraph :bind "get_rid" :hash
  2944877500)
 rid)

(defgmethod
 (text-paragraph+get-line-rid :class 'text-paragraph :bind "get_line_rid" :hash
  495598643)
 rid (line int))

(defgmethod
 (text-paragraph+get-dropcap-rid :class 'text-paragraph :bind "get_dropcap_rid"
  :hash 2944877500)
 rid)

(defgmethod
 (text-paragraph+get-range :class 'text-paragraph :bind "get_range" :hash
  3690982128)
 vector-2i)

(defgmethod
 (text-paragraph+get-line-count :class 'text-paragraph :bind "get_line_count"
  :hash 3905245786)
 int)

(defgmethod
 (text-paragraph+set-max-lines-visible :class 'text-paragraph :bind
  "set_max_lines_visible" :hash 1286410249)
 :void (max-lines-visible int))

(defgmethod
 (text-paragraph+get-max-lines-visible :class 'text-paragraph :bind
  "get_max_lines_visible" :hash 3905245786)
 int)

(defgmethod
 (text-paragraph+set-line-spacing :class 'text-paragraph :bind
  "set_line_spacing" :hash 373806689)
 :void (line-spacing float))

(defgmethod
 (text-paragraph+get-line-spacing :class 'text-paragraph :bind
  "get_line_spacing" :hash 1740695150)
 float)

(defgmethod
 (text-paragraph+get-line-objects :class 'text-paragraph :bind
  "get_line_objects" :hash 663333327)
 array (line int))

(defgmethod
 (text-paragraph+get-line-object-rect :class 'text-paragraph :bind
  "get_line_object_rect" :hash 204315017)
 rect-2 (line int) (key variant))

(defgmethod
 (text-paragraph+get-line-size :class 'text-paragraph :bind "get_line_size"
  :hash 2299179447)
 vector-2 (line int))

(defgmethod
 (text-paragraph+get-line-range :class 'text-paragraph :bind "get_line_range"
  :hash 880721226)
 vector-2i (line int))

(defgmethod
 (text-paragraph+get-line-ascent :class 'text-paragraph :bind "get_line_ascent"
  :hash 2339986948)
 float (line int))

(defgmethod
 (text-paragraph+get-line-descent :class 'text-paragraph :bind
  "get_line_descent" :hash 2339986948)
 float (line int))

(defgmethod
 (text-paragraph+get-line-width :class 'text-paragraph :bind "get_line_width"
  :hash 2339986948)
 float (line int))

(defgmethod
 (text-paragraph+get-line-underline-position :class 'text-paragraph :bind
  "get_line_underline_position" :hash 2339986948)
 float (line int))

(defgmethod
 (text-paragraph+get-line-underline-thickness :class 'text-paragraph :bind
  "get_line_underline_thickness" :hash 2339986948)
 float (line int))

(defgmethod
 (text-paragraph+get-dropcap-size :class 'text-paragraph :bind
  "get_dropcap_size" :hash 3341600327)
 vector-2)

(defgmethod
 (text-paragraph+get-dropcap-lines :class 'text-paragraph :bind
  "get_dropcap_lines" :hash 3905245786)
 int)

(defgmethod
 (text-paragraph+draw :class 'text-paragraph :bind "draw" :hash 1492808103)
 :void (canvas rid) (pos vector-2) (color color) (dc-color color)
 (oversampling float))

(defgmethod
 (text-paragraph+draw-outline :class 'text-paragraph :bind "draw_outline" :hash
  3820500590)
 :void (canvas rid) (pos vector-2) (outline-size int) (color color)
 (dc-color color) (oversampling float))

(defgmethod
 (text-paragraph+draw-line :class 'text-paragraph :bind "draw_line" :hash
  828033758)
 :void (canvas rid) (pos vector-2) (line int) (color color)
 (oversampling float))

(defgmethod
 (text-paragraph+draw-line-outline :class 'text-paragraph :bind
  "draw_line_outline" :hash 2822696703)
 :void (canvas rid) (pos vector-2) (line int) (outline-size int) (color color)
 (oversampling float))

(defgmethod
 (text-paragraph+draw-dropcap :class 'text-paragraph :bind "draw_dropcap" :hash
  3625105422)
 :void (canvas rid) (pos vector-2) (color color) (oversampling float))

(defgmethod
 (text-paragraph+draw-dropcap-outline :class 'text-paragraph :bind
  "draw_dropcap_outline" :hash 2592177763)
 :void (canvas rid) (pos vector-2) (outline-size int) (color color)
 (oversampling float))

(defgmethod
 (text-paragraph+hit-test :class 'text-paragraph :bind "hit_test" :hash
  3820158470)
 int (coords vector-2))