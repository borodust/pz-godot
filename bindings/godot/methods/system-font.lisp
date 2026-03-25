(common-lisp:in-package :%godot)


(defgmethod
 (system-font+set-antialiasing :class 'system-font :bind "set_antialiasing"
  :hash 1669900)
 :void (antialiasing text-server+font-antialiasing))

(defgmethod
 (system-font+get-antialiasing :class 'system-font :bind "get_antialiasing"
  :hash 4262718649)
 text-server+font-antialiasing)

(defgmethod
 (system-font+set-disable-embedded-bitmaps :class 'system-font :bind
  "set_disable_embedded_bitmaps" :hash 2586408642)
 :void (disable-embedded-bitmaps bool))

(defgmethod
 (system-font+get-disable-embedded-bitmaps :class 'system-font :bind
  "get_disable_embedded_bitmaps" :hash 36873697)
 bool)

(defgmethod
 (system-font+set-generate-mipmaps :class 'system-font :bind
  "set_generate_mipmaps" :hash 2586408642)
 :void (generate-mipmaps bool))

(defgmethod
 (system-font+get-generate-mipmaps :class 'system-font :bind
  "get_generate_mipmaps" :hash 36873697)
 bool)

(defgmethod
 (system-font+set-allow-system-fallback :class 'system-font :bind
  "set_allow_system_fallback" :hash 2586408642)
 :void (allow-system-fallback bool))

(defgmethod
 (system-font+is-allow-system-fallback :class 'system-font :bind
  "is_allow_system_fallback" :hash 36873697)
 bool)

(defgmethod
 (system-font+set-force-autohinter :class 'system-font :bind
  "set_force_autohinter" :hash 2586408642)
 :void (force-autohinter bool))

(defgmethod
 (system-font+is-force-autohinter :class 'system-font :bind
  "is_force_autohinter" :hash 36873697)
 bool)

(defgmethod
 (system-font+set-modulate-color-glyphs :class 'system-font :bind
  "set_modulate_color_glyphs" :hash 2586408642)
 :void (modulate bool))

(defgmethod
 (system-font+is-modulate-color-glyphs :class 'system-font :bind
  "is_modulate_color_glyphs" :hash 36873697)
 bool)

(defgmethod
 (system-font+set-hinting :class 'system-font :bind "set_hinting" :hash
  1827459492)
 :void (hinting text-server+hinting))

(defgmethod
 (system-font+get-hinting :class 'system-font :bind "get_hinting" :hash
  3683214614)
 text-server+hinting)

(defgmethod
 (system-font+set-subpixel-positioning :class 'system-font :bind
  "set_subpixel_positioning" :hash 4225742182)
 :void (subpixel-positioning text-server+subpixel-positioning))

(defgmethod
 (system-font+get-subpixel-positioning :class 'system-font :bind
  "get_subpixel_positioning" :hash 1069238588)
 text-server+subpixel-positioning)

(defgmethod
 (system-font+set-keep-rounding-remainders :class 'system-font :bind
  "set_keep_rounding_remainders" :hash 2586408642)
 :void (keep-rounding-remainders bool))

(defgmethod
 (system-font+get-keep-rounding-remainders :class 'system-font :bind
  "get_keep_rounding_remainders" :hash 36873697)
 bool)

(defgmethod
 (system-font+set-multichannel-signed-distance-field :class 'system-font :bind
  "set_multichannel_signed_distance_field" :hash 2586408642)
 :void (msdf bool))

(defgmethod
 (system-font+is-multichannel-signed-distance-field :class 'system-font :bind
  "is_multichannel_signed_distance_field" :hash 36873697)
 bool)

(defgmethod
 (system-font+set-msdf-pixel-range :class 'system-font :bind
  "set_msdf_pixel_range" :hash 1286410249)
 :void (msdf-pixel-range int))

(defgmethod
 (system-font+get-msdf-pixel-range :class 'system-font :bind
  "get_msdf_pixel_range" :hash 3905245786)
 int)

(defgmethod
 (system-font+set-msdf-size :class 'system-font :bind "set_msdf_size" :hash
  1286410249)
 :void (msdf-size int))

(defgmethod
 (system-font+get-msdf-size :class 'system-font :bind "get_msdf_size" :hash
  3905245786)
 int)

(defgmethod
 (system-font+set-oversampling :class 'system-font :bind "set_oversampling"
  :hash 373806689)
 :void (oversampling float))

(defgmethod
 (system-font+get-oversampling :class 'system-font :bind "get_oversampling"
  :hash 1740695150)
 float)

(defgmethod
 (system-font+get-font-names :class 'system-font :bind "get_font_names" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (system-font+set-font-names :class 'system-font :bind "set_font_names" :hash
  4015028928)
 :void (names packed-string-array))

(defgmethod
 (system-font+get-font-italic :class 'system-font :bind "get_font_italic" :hash
  36873697)
 bool)

(defgmethod
 (system-font+set-font-italic :class 'system-font :bind "set_font_italic" :hash
  2586408642)
 :void (italic bool))

(defgmethod
 (system-font+set-font-weight :class 'system-font :bind "set_font_weight" :hash
  1286410249)
 :void (weight int))

(defgmethod
 (system-font+set-font-stretch :class 'system-font :bind "set_font_stretch"
  :hash 1286410249)
 :void (stretch int))