(common-lisp:in-package :%godot)


(defgmethod
 (font+set-fallbacks :class 'font :bind "set_fallbacks" :hash 381264803) :void
 (fallbacks array))

(defgmethod
 (font+get-fallbacks :class 'font :bind "get_fallbacks" :hash 3995934104) array)

(defgmethod
 (font+find-variation :class 'font :bind "find_variation" :hash 3275867622) rid
 (variation-coordinates dictionary) (face-index int) (strength float)
 (transform transform-2d) (spacing-top int) (spacing-bottom int)
 (spacing-space int) (spacing-glyph int) (baseline-offset float)
 (palette-index int) (custom-colors packed-color-array))

(defgmethod (font+get-rids :class 'font :bind "get_rids" :hash 3995934104)
 array)

(defgmethod (font+get-height :class 'font :bind "get_height" :hash 378113874)
 float (font-size int))

(defgmethod (font+get-ascent :class 'font :bind "get_ascent" :hash 378113874)
 float (font-size int))

(defgmethod (font+get-descent :class 'font :bind "get_descent" :hash 378113874)
 float (font-size int))

(defgmethod
 (font+get-underline-position :class 'font :bind "get_underline_position" :hash
  378113874)
 float (font-size int))

(defgmethod
 (font+get-underline-thickness :class 'font :bind "get_underline_thickness"
  :hash 378113874)
 float (font-size int))

(defgmethod
 (font+get-font-name :class 'font :bind "get_font_name" :hash 201670096) string)

(defgmethod
 (font+get-font-style-name :class 'font :bind "get_font_style_name" :hash
  201670096)
 string)

(defgmethod
 (font+get-ot-name-strings :class 'font :bind "get_ot_name_strings" :hash
  3102165223)
 dictionary)

(defgmethod
 (font+get-font-style :class 'font :bind "get_font_style" :hash 2520224254)
 text-server+font-style)

(defgmethod
 (font+get-font-weight :class 'font :bind "get_font_weight" :hash 3905245786)
 int)

(defgmethod
 (font+get-font-stretch :class 'font :bind "get_font_stretch" :hash 3905245786)
 int)

(defgmethod
 (font+get-palette-count :class 'font :bind "get_palette_count" :hash
  3905245786)
 int)

(defgmethod
 (font+get-palette-name :class 'font :bind "get_palette_name" :hash 844755477)
 string (index int))

(defgmethod
 (font+get-palette-colors :class 'font :bind "get_palette_colors" :hash
  2552048864)
 packed-color-array (index int))

(defgmethod
 (font+get-spacing :class 'font :bind "get_spacing" :hash 1310880908) int
 (spacing text-server+spacing-type))

(defgmethod
 (font+get-opentype-features :class 'font :bind "get_opentype_features" :hash
  3102165223)
 dictionary)

(defgmethod
 (font+set-cache-capacity :class 'font :bind "set_cache_capacity" :hash
  3937882851)
 :void (single-line int) (multi-line int))

(defgmethod
 (font+get-string-size :class 'font :bind "get_string_size" :hash 1868866121)
 vector-2 (text string) (alignment horizontal-alignment) (width float)
 (font-size int) (justification-flags text-server+justification-flag)
 (direction text-server+direction) (orientation text-server+orientation))

(defgmethod
 (font+get-multiline-string-size :class 'font :bind "get_multiline_string_size"
  :hash 519636710)
 vector-2 (text string) (alignment horizontal-alignment) (width float)
 (font-size int) (max-lines int) (brk-flags text-server+line-break-flag)
 (justification-flags text-server+justification-flag)
 (direction text-server+direction) (orientation text-server+orientation))

(defgmethod
 (font+draw-string :class 'font :bind "draw_string" :hash 1976686372) :void
 (canvas-item rid) (pos vector-2) (text string)
 (alignment horizontal-alignment) (width float) (font-size int)
 (modulate color) (justification-flags text-server+justification-flag)
 (direction text-server+direction) (orientation text-server+orientation)
 (oversampling float))

(defgmethod
 (font+draw-multiline-string :class 'font :bind "draw_multiline_string" :hash
  2686601589)
 :void (canvas-item rid) (pos vector-2) (text string)
 (alignment horizontal-alignment) (width float) (font-size int) (max-lines int)
 (modulate color) (brk-flags text-server+line-break-flag)
 (justification-flags text-server+justification-flag)
 (direction text-server+direction) (orientation text-server+orientation)
 (oversampling float))

(defgmethod
 (font+draw-string-outline :class 'font :bind "draw_string_outline" :hash
  701417663)
 :void (canvas-item rid) (pos vector-2) (text string)
 (alignment horizontal-alignment) (width float) (font-size int) (size int)
 (modulate color) (justification-flags text-server+justification-flag)
 (direction text-server+direction) (orientation text-server+orientation)
 (oversampling float))

(defgmethod
 (font+draw-multiline-string-outline :class 'font :bind
  "draw_multiline_string_outline" :hash 4147839237)
 :void (canvas-item rid) (pos vector-2) (text string)
 (alignment horizontal-alignment) (width float) (font-size int) (max-lines int)
 (size int) (modulate color) (brk-flags text-server+line-break-flag)
 (justification-flags text-server+justification-flag)
 (direction text-server+direction) (orientation text-server+orientation)
 (oversampling float))

(defgmethod
 (font+get-char-size :class 'font :bind "get_char_size" :hash 3016396712)
 vector-2 (char int) (font-size int))

(defgmethod (font+draw-char :class 'font :bind "draw_char" :hash 3500170256)
 float (canvas-item rid) (pos vector-2) (char int) (font-size int)
 (modulate color) (oversampling float))

(defgmethod
 (font+draw-char-outline :class 'font :bind "draw_char_outline" :hash
  1684114874)
 float (canvas-item rid) (pos vector-2) (char int) (font-size int) (size int)
 (modulate color) (oversampling float))

(defgmethod (font+has-char :class 'font :bind "has_char" :hash 1116898809) bool
 (char int))

(defgmethod
 (font+get-supported-chars :class 'font :bind "get_supported_chars" :hash
  201670096)
 string)

(defgmethod
 (font+is-language-supported :class 'font :bind "is_language_supported" :hash
  3927539163)
 bool (language string))

(defgmethod
 (font+is-script-supported :class 'font :bind "is_script_supported" :hash
  3927539163)
 bool (script string))

(defgmethod
 (font+get-supported-feature-list :class 'font :bind
  "get_supported_feature_list" :hash 3102165223)
 dictionary)

(defgmethod
 (font+get-supported-variation-list :class 'font :bind
  "get_supported_variation_list" :hash 3102165223)
 dictionary)

(defgmethod
 (font+get-face-count :class 'font :bind "get_face_count" :hash 3905245786) int)