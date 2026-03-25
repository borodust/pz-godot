(common-lisp:in-package :%godot)


(defgmethod
 (char-fxtransform+get-transform :class 'char-fxtransform :bind "get_transform"
  :hash 3761352769)
 transform-2d)

(defgmethod
 (char-fxtransform+set-transform :class 'char-fxtransform :bind "set_transform"
  :hash 2761652528)
 :void (transform transform-2d))

(defgmethod
 (char-fxtransform+get-range :class 'char-fxtransform :bind "get_range" :hash
  2741790807)
 vector-2i)

(defgmethod
 (char-fxtransform+set-range :class 'char-fxtransform :bind "set_range" :hash
  1130785943)
 :void (range vector-2i))

(defgmethod
 (char-fxtransform+get-elapsed-time :class 'char-fxtransform :bind
  "get_elapsed_time" :hash 191475506)
 float)

(defgmethod
 (char-fxtransform+set-elapsed-time :class 'char-fxtransform :bind
  "set_elapsed_time" :hash 373806689)
 :void (time float))

(defgmethod
 (char-fxtransform+is-visible :class 'char-fxtransform :bind "is_visible" :hash
  2240911060)
 bool)

(defgmethod
 (char-fxtransform+set-visibility :class 'char-fxtransform :bind
  "set_visibility" :hash 2586408642)
 :void (visibility bool))

(defgmethod
 (char-fxtransform+is-outline :class 'char-fxtransform :bind "is_outline" :hash
  2240911060)
 bool)

(defgmethod
 (char-fxtransform+set-outline :class 'char-fxtransform :bind "set_outline"
  :hash 2586408642)
 :void (outline bool))

(defgmethod
 (char-fxtransform+get-offset :class 'char-fxtransform :bind "get_offset" :hash
  1497962370)
 vector-2)

(defgmethod
 (char-fxtransform+set-offset :class 'char-fxtransform :bind "set_offset" :hash
  743155724)
 :void (offset vector-2))

(defgmethod
 (char-fxtransform+get-color :class 'char-fxtransform :bind "get_color" :hash
  3200896285)
 color)

(defgmethod
 (char-fxtransform+set-color :class 'char-fxtransform :bind "set_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (char-fxtransform+get-environment :class 'char-fxtransform :bind
  "get_environment" :hash 2382534195)
 dictionary)

(defgmethod
 (char-fxtransform+set-environment :class 'char-fxtransform :bind
  "set_environment" :hash 4155329257)
 :void (environment dictionary))

(defgmethod
 (char-fxtransform+get-glyph-index :class 'char-fxtransform :bind
  "get_glyph_index" :hash 3905245786)
 int)

(defgmethod
 (char-fxtransform+set-glyph-index :class 'char-fxtransform :bind
  "set_glyph_index" :hash 1286410249)
 :void (glyph-index int))

(defgmethod
 (char-fxtransform+get-relative-index :class 'char-fxtransform :bind
  "get_relative_index" :hash 3905245786)
 int)

(defgmethod
 (char-fxtransform+set-relative-index :class 'char-fxtransform :bind
  "set_relative_index" :hash 1286410249)
 :void (relative-index int))

(defgmethod
 (char-fxtransform+get-glyph-count :class 'char-fxtransform :bind
  "get_glyph_count" :hash 3905245786)
 int)

(defgmethod
 (char-fxtransform+set-glyph-count :class 'char-fxtransform :bind
  "set_glyph_count" :hash 1286410249)
 :void (glyph-count int))

(defgmethod
 (char-fxtransform+get-glyph-flags :class 'char-fxtransform :bind
  "get_glyph_flags" :hash 3905245786)
 int)

(defgmethod
 (char-fxtransform+set-glyph-flags :class 'char-fxtransform :bind
  "set_glyph_flags" :hash 1286410249)
 :void (glyph-flags int))

(defgmethod
 (char-fxtransform+get-font :class 'char-fxtransform :bind "get_font" :hash
  2944877500)
 rid)

(defgmethod
 (char-fxtransform+set-font :class 'char-fxtransform :bind "set_font" :hash
  2722037293)
 :void (font rid))