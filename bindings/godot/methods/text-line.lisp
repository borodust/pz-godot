(common-lisp:in-package :%godot)


(defgmethod (text-line+clear :class 'text-line :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (text-line+duplicate :class 'text-line :bind "duplicate" :hash 1912703884)
 text-line)

(defgmethod
 (text-line+set-direction :class 'text-line :bind "set_direction" :hash
  1418190634)
 :void (direction text-server+direction))

(defgmethod
 (text-line+get-direction :class 'text-line :bind "get_direction" :hash
  2516697328)
 text-server+direction)

(defgmethod
 (text-line+get-inferred-direction :class 'text-line :bind
  "get_inferred_direction" :hash 2516697328)
 text-server+direction)

(defgmethod
 (text-line+set-orientation :class 'text-line :bind "set_orientation" :hash
  42823726)
 :void (orientation text-server+orientation))

(defgmethod
 (text-line+get-orientation :class 'text-line :bind "get_orientation" :hash
  175768116)
 text-server+orientation)

(defgmethod
 (text-line+set-preserve-invalid :class 'text-line :bind "set_preserve_invalid"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-line+get-preserve-invalid :class 'text-line :bind "get_preserve_invalid"
  :hash 36873697)
 bool)

(defgmethod
 (text-line+set-preserve-control :class 'text-line :bind "set_preserve_control"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-line+get-preserve-control :class 'text-line :bind "get_preserve_control"
  :hash 36873697)
 bool)

(defgmethod
 (text-line+set-bidi-override :class 'text-line :bind "set_bidi_override" :hash
  381264803)
 :void (override array))

(defgmethod
 (text-line+add-string :class 'text-line :bind "add_string" :hash 621426851)
 bool (text string) (font font) (font-size int) (language string)
 (meta variant))

(defgmethod
 (text-line+add-object :class 'text-line :bind "add_object" :hash 1316529304)
 bool (key variant) (size vector-2) (inline-align inline-alignment)
 (length int) (baseline float))

(defgmethod
 (text-line+resize-object :class 'text-line :bind "resize_object" :hash
  2095776372)
 bool (key variant) (size vector-2) (inline-align inline-alignment)
 (baseline float))

(defgmethod
 (text-line+has-object :class 'text-line :bind "has_object" :hash 77467830)
 bool (key variant))

(defgmethod
 (text-line+set-width :class 'text-line :bind "set_width" :hash 373806689)
 :void (width float))

(defgmethod
 (text-line+get-width :class 'text-line :bind "get_width" :hash 1740695150)
 float)

(defgmethod
 (text-line+set-horizontal-alignment :class 'text-line :bind
  "set_horizontal_alignment" :hash 2312603777)
 :void (alignment horizontal-alignment))

(defgmethod
 (text-line+get-horizontal-alignment :class 'text-line :bind
  "get_horizontal_alignment" :hash 341400642)
 horizontal-alignment)

(defgmethod
 (text-line+tab-align :class 'text-line :bind "tab_align" :hash 2899603908)
 :void (tab-stops packed-float-32array))

(defgmethod
 (text-line+set-flags :class 'text-line :bind "set_flags" :hash 2877345813)
 :void (flags text-server+justification-flag))

(defgmethod
 (text-line+get-flags :class 'text-line :bind "get_flags" :hash 1583363614)
 text-server+justification-flag)

(defgmethod
 (text-line+set-text-overrun-behavior :class 'text-line :bind
  "set_text_overrun_behavior" :hash 1008890932)
 :void (overrun-behavior text-server+overrun-behavior))

(defgmethod
 (text-line+get-text-overrun-behavior :class 'text-line :bind
  "get_text_overrun_behavior" :hash 3779142101)
 text-server+overrun-behavior)

(defgmethod
 (text-line+set-ellipsis-char :class 'text-line :bind "set_ellipsis_char" :hash
  83702148)
 :void (char string))

(defgmethod
 (text-line+get-ellipsis-char :class 'text-line :bind "get_ellipsis_char" :hash
  201670096)
 string)

(defgmethod
 (text-line+get-objects :class 'text-line :bind "get_objects" :hash 3995934104)
 array)

(defgmethod
 (text-line+get-object-rect :class 'text-line :bind "get_object_rect" :hash
  1742700391)
 rect-2 (key variant))

(defgmethod
 (text-line+get-size :class 'text-line :bind "get_size" :hash 3341600327)
 vector-2)

(defgmethod
 (text-line+get-rid :class 'text-line :bind "get_rid" :hash 2944877500) rid)

(defgmethod
 (text-line+get-line-ascent :class 'text-line :bind "get_line_ascent" :hash
  1740695150)
 float)

(defgmethod
 (text-line+get-line-descent :class 'text-line :bind "get_line_descent" :hash
  1740695150)
 float)

(defgmethod
 (text-line+get-line-width :class 'text-line :bind "get_line_width" :hash
  1740695150)
 float)

(defgmethod
 (text-line+get-line-underline-position :class 'text-line :bind
  "get_line_underline_position" :hash 1740695150)
 float)

(defgmethod
 (text-line+get-line-underline-thickness :class 'text-line :bind
  "get_line_underline_thickness" :hash 1740695150)
 float)

(defgmethod (text-line+draw :class 'text-line :bind "draw" :hash 3625105422)
 :void (canvas rid) (pos vector-2) (color color) (oversampling float))

(defgmethod
 (text-line+draw-outline :class 'text-line :bind "draw_outline" :hash
  2592177763)
 :void (canvas rid) (pos vector-2) (outline-size int) (color color)
 (oversampling float))

(defgmethod
 (text-line+hit-test :class 'text-line :bind "hit_test" :hash 2401831903) int
 (coords float))