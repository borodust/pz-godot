(common-lisp:in-package :%godot)


(defgmethod
 (font-file+load-bitmap-font :class 'font-file :bind "load_bitmap_font" :hash
  166001499)
 error (path string))

(defgmethod
 (font-file+load-dynamic-font :class 'font-file :bind "load_dynamic_font" :hash
  166001499)
 error (path string))

(defgmethod
 (font-file+set-data :class 'font-file :bind "set_data" :hash 2971499966) :void
 (data packed-byte-array))

(defgmethod
 (font-file+get-data :class 'font-file :bind "get_data" :hash 2362200018)
 packed-byte-array)

(defgmethod
 (font-file+set-font-name :class 'font-file :bind "set_font_name" :hash
  83702148)
 :void (name string))

(defgmethod
 (font-file+set-font-style-name :class 'font-file :bind "set_font_style_name"
  :hash 83702148)
 :void (name string))

(defgmethod
 (font-file+set-font-style :class 'font-file :bind "set_font_style" :hash
  918070724)
 :void (style text-server+font-style))

(defgmethod
 (font-file+set-font-weight :class 'font-file :bind "set_font_weight" :hash
  1286410249)
 :void (weight int))

(defgmethod
 (font-file+set-font-stretch :class 'font-file :bind "set_font_stretch" :hash
  1286410249)
 :void (stretch int))

(defgmethod
 (font-file+set-antialiasing :class 'font-file :bind "set_antialiasing" :hash
  1669900)
 :void (antialiasing text-server+font-antialiasing))

(defgmethod
 (font-file+get-antialiasing :class 'font-file :bind "get_antialiasing" :hash
  4262718649)
 text-server+font-antialiasing)

(defgmethod
 (font-file+set-disable-embedded-bitmaps :class 'font-file :bind
  "set_disable_embedded_bitmaps" :hash 2586408642)
 :void (disable-embedded-bitmaps bool))

(defgmethod
 (font-file+get-disable-embedded-bitmaps :class 'font-file :bind
  "get_disable_embedded_bitmaps" :hash 36873697)
 bool)

(defgmethod
 (font-file+set-generate-mipmaps :class 'font-file :bind "set_generate_mipmaps"
  :hash 2586408642)
 :void (generate-mipmaps bool))

(defgmethod
 (font-file+get-generate-mipmaps :class 'font-file :bind "get_generate_mipmaps"
  :hash 36873697)
 bool)

(defgmethod
 (font-file+set-multichannel-signed-distance-field :class 'font-file :bind
  "set_multichannel_signed_distance_field" :hash 2586408642)
 :void (msdf bool))

(defgmethod
 (font-file+is-multichannel-signed-distance-field :class 'font-file :bind
  "is_multichannel_signed_distance_field" :hash 36873697)
 bool)

(defgmethod
 (font-file+set-msdf-pixel-range :class 'font-file :bind "set_msdf_pixel_range"
  :hash 1286410249)
 :void (msdf-pixel-range int))

(defgmethod
 (font-file+get-msdf-pixel-range :class 'font-file :bind "get_msdf_pixel_range"
  :hash 3905245786)
 int)

(defgmethod
 (font-file+set-msdf-size :class 'font-file :bind "set_msdf_size" :hash
  1286410249)
 :void (msdf-size int))

(defgmethod
 (font-file+get-msdf-size :class 'font-file :bind "get_msdf_size" :hash
  3905245786)
 int)

(defgmethod
 (font-file+set-fixed-size :class 'font-file :bind "set_fixed_size" :hash
  1286410249)
 :void (fixed-size int))

(defgmethod
 (font-file+get-fixed-size :class 'font-file :bind "get_fixed_size" :hash
  3905245786)
 int)

(defgmethod
 (font-file+set-fixed-size-scale-mode :class 'font-file :bind
  "set_fixed_size_scale_mode" :hash 1660989956)
 :void (fixed-size-scale-mode text-server+fixed-size-scale-mode))

(defgmethod
 (font-file+get-fixed-size-scale-mode :class 'font-file :bind
  "get_fixed_size_scale_mode" :hash 753873478)
 text-server+fixed-size-scale-mode)

(defgmethod
 (font-file+set-allow-system-fallback :class 'font-file :bind
  "set_allow_system_fallback" :hash 2586408642)
 :void (allow-system-fallback bool))

(defgmethod
 (font-file+is-allow-system-fallback :class 'font-file :bind
  "is_allow_system_fallback" :hash 36873697)
 bool)

(defgmethod
 (font-file+set-force-autohinter :class 'font-file :bind "set_force_autohinter"
  :hash 2586408642)
 :void (force-autohinter bool))

(defgmethod
 (font-file+is-force-autohinter :class 'font-file :bind "is_force_autohinter"
  :hash 36873697)
 bool)

(defgmethod
 (font-file+set-modulate-color-glyphs :class 'font-file :bind
  "set_modulate_color_glyphs" :hash 2586408642)
 :void (modulate bool))

(defgmethod
 (font-file+is-modulate-color-glyphs :class 'font-file :bind
  "is_modulate_color_glyphs" :hash 36873697)
 bool)

(defgmethod
 (font-file+set-hinting :class 'font-file :bind "set_hinting" :hash 1827459492)
 :void (hinting text-server+hinting))

(defgmethod
 (font-file+get-hinting :class 'font-file :bind "get_hinting" :hash 3683214614)
 text-server+hinting)

(defgmethod
 (font-file+set-subpixel-positioning :class 'font-file :bind
  "set_subpixel_positioning" :hash 4225742182)
 :void (subpixel-positioning text-server+subpixel-positioning))

(defgmethod
 (font-file+get-subpixel-positioning :class 'font-file :bind
  "get_subpixel_positioning" :hash 1069238588)
 text-server+subpixel-positioning)

(defgmethod
 (font-file+set-keep-rounding-remainders :class 'font-file :bind
  "set_keep_rounding_remainders" :hash 2586408642)
 :void (keep-rounding-remainders bool))

(defgmethod
 (font-file+get-keep-rounding-remainders :class 'font-file :bind
  "get_keep_rounding_remainders" :hash 36873697)
 bool)

(defgmethod
 (font-file+set-oversampling :class 'font-file :bind "set_oversampling" :hash
  373806689)
 :void (oversampling float))

(defgmethod
 (font-file+get-oversampling :class 'font-file :bind "get_oversampling" :hash
  1740695150)
 float)

(defgmethod
 (font-file+get-cache-count :class 'font-file :bind "get_cache_count" :hash
  3905245786)
 int)

(defgmethod
 (font-file+clear-cache :class 'font-file :bind "clear_cache" :hash 3218959716)
 :void)

(defgmethod
 (font-file+remove-cache :class 'font-file :bind "remove_cache" :hash
  1286410249)
 :void (cache-index int))

(defgmethod
 (font-file+get-size-cache-list :class 'font-file :bind "get_size_cache_list"
  :hash 663333327)
 array (cache-index int))

(defgmethod
 (font-file+clear-size-cache :class 'font-file :bind "clear_size_cache" :hash
  1286410249)
 :void (cache-index int))

(defgmethod
 (font-file+remove-size-cache :class 'font-file :bind "remove_size_cache" :hash
  2311374912)
 :void (cache-index int) (size vector-2i))

(defgmethod
 (font-file+set-variation-coordinates :class 'font-file :bind
  "set_variation_coordinates" :hash 64545446)
 :void (cache-index int) (variation-coordinates dictionary))

(defgmethod
 (font-file+get-variation-coordinates :class 'font-file :bind
  "get_variation_coordinates" :hash 3485342025)
 dictionary (cache-index int))

(defgmethod
 (font-file+set-embolden :class 'font-file :bind "set_embolden" :hash
  1602489585)
 :void (cache-index int) (strength float))

(defgmethod
 (font-file+get-embolden :class 'font-file :bind "get_embolden" :hash
  2339986948)
 float (cache-index int))

(defgmethod
 (font-file+set-transform :class 'font-file :bind "set_transform" :hash
  30160968)
 :void (cache-index int) (transform transform-2d))

(defgmethod
 (font-file+get-transform :class 'font-file :bind "get_transform" :hash
  3836996910)
 transform-2d (cache-index int))

(defgmethod
 (font-file+set-extra-spacing :class 'font-file :bind "set_extra_spacing" :hash
  62942285)
 :void (cache-index int) (spacing text-server+spacing-type) (value int))

(defgmethod
 (font-file+get-extra-spacing :class 'font-file :bind "get_extra_spacing" :hash
  1924257185)
 int (cache-index int) (spacing text-server+spacing-type))

(defgmethod
 (font-file+set-extra-baseline-offset :class 'font-file :bind
  "set_extra_baseline_offset" :hash 1602489585)
 :void (cache-index int) (baseline-offset float))

(defgmethod
 (font-file+get-extra-baseline-offset :class 'font-file :bind
  "get_extra_baseline_offset" :hash 2339986948)
 float (cache-index int))

(defgmethod
 (font-file+set-face-index :class 'font-file :bind "set_face_index" :hash
  3937882851)
 :void (cache-index int) (face-index int))

(defgmethod
 (font-file+get-face-index :class 'font-file :bind "get_face_index" :hash
  923996154)
 int (cache-index int))

(defgmethod
 (font-file+set-cache-ascent :class 'font-file :bind "set_cache_ascent" :hash
  3506521499)
 :void (cache-index int) (size int) (ascent float))

(defgmethod
 (font-file+get-cache-ascent :class 'font-file :bind "get_cache_ascent" :hash
  3085491603)
 float (cache-index int) (size int))

(defgmethod
 (font-file+set-cache-descent :class 'font-file :bind "set_cache_descent" :hash
  3506521499)
 :void (cache-index int) (size int) (descent float))

(defgmethod
 (font-file+get-cache-descent :class 'font-file :bind "get_cache_descent" :hash
  3085491603)
 float (cache-index int) (size int))

(defgmethod
 (font-file+set-cache-underline-position :class 'font-file :bind
  "set_cache_underline_position" :hash 3506521499)
 :void (cache-index int) (size int) (underline-position float))

(defgmethod
 (font-file+get-cache-underline-position :class 'font-file :bind
  "get_cache_underline_position" :hash 3085491603)
 float (cache-index int) (size int))

(defgmethod
 (font-file+set-cache-underline-thickness :class 'font-file :bind
  "set_cache_underline_thickness" :hash 3506521499)
 :void (cache-index int) (size int) (underline-thickness float))

(defgmethod
 (font-file+get-cache-underline-thickness :class 'font-file :bind
  "get_cache_underline_thickness" :hash 3085491603)
 float (cache-index int) (size int))

(defgmethod
 (font-file+set-cache-scale :class 'font-file :bind "set_cache_scale" :hash
  3506521499)
 :void (cache-index int) (size int) (scale float))

(defgmethod
 (font-file+get-cache-scale :class 'font-file :bind "get_cache_scale" :hash
  3085491603)
 float (cache-index int) (size int))

(defgmethod
 (font-file+get-texture-count :class 'font-file :bind "get_texture_count" :hash
  1987661582)
 int (cache-index int) (size vector-2i))

(defgmethod
 (font-file+clear-textures :class 'font-file :bind "clear_textures" :hash
  2311374912)
 :void (cache-index int) (size vector-2i))

(defgmethod
 (font-file+remove-texture :class 'font-file :bind "remove_texture" :hash
  2328951467)
 :void (cache-index int) (size vector-2i) (texture-index int))

(defgmethod
 (font-file+set-texture-image :class 'font-file :bind "set_texture_image" :hash
  4157974066)
 :void (cache-index int) (size vector-2i) (texture-index int) (image image))

(defgmethod
 (font-file+get-texture-image :class 'font-file :bind "get_texture_image" :hash
  3878418953)
 image (cache-index int) (size vector-2i) (texture-index int))

(defgmethod
 (font-file+set-texture-offsets :class 'font-file :bind "set_texture_offsets"
  :hash 2849993437)
 :void (cache-index int) (size vector-2i) (texture-index int)
 (offset packed-int-32array))

(defgmethod
 (font-file+get-texture-offsets :class 'font-file :bind "get_texture_offsets"
  :hash 3703444828)
 packed-int-32array (cache-index int) (size vector-2i) (texture-index int))

(defgmethod
 (font-file+get-glyph-list :class 'font-file :bind "get_glyph_list" :hash
  681709689)
 packed-int-32array (cache-index int) (size vector-2i))

(defgmethod
 (font-file+clear-glyphs :class 'font-file :bind "clear_glyphs" :hash
  2311374912)
 :void (cache-index int) (size vector-2i))

(defgmethod
 (font-file+remove-glyph :class 'font-file :bind "remove_glyph" :hash
  2328951467)
 :void (cache-index int) (size vector-2i) (glyph int))

(defgmethod
 (font-file+set-glyph-advance :class 'font-file :bind "set_glyph_advance" :hash
  947991729)
 :void (cache-index int) (size int) (glyph int) (advance vector-2))

(defgmethod
 (font-file+get-glyph-advance :class 'font-file :bind "get_glyph_advance" :hash
  1601573536)
 vector-2 (cache-index int) (size int) (glyph int))

(defgmethod
 (font-file+set-glyph-offset :class 'font-file :bind "set_glyph_offset" :hash
  921719850)
 :void (cache-index int) (size vector-2i) (glyph int) (offset vector-2))

(defgmethod
 (font-file+get-glyph-offset :class 'font-file :bind "get_glyph_offset" :hash
  3205412300)
 vector-2 (cache-index int) (size vector-2i) (glyph int))

(defgmethod
 (font-file+set-glyph-size :class 'font-file :bind "set_glyph_size" :hash
  921719850)
 :void (cache-index int) (size vector-2i) (glyph int) (gl-size vector-2))

(defgmethod
 (font-file+get-glyph-size :class 'font-file :bind "get_glyph_size" :hash
  3205412300)
 vector-2 (cache-index int) (size vector-2i) (glyph int))

(defgmethod
 (font-file+set-glyph-uv-rect :class 'font-file :bind "set_glyph_uv_rect" :hash
  3821620992)
 :void (cache-index int) (size vector-2i) (glyph int) (uv-rect rect-2))

(defgmethod
 (font-file+get-glyph-uv-rect :class 'font-file :bind "get_glyph_uv_rect" :hash
  3927917900)
 rect-2 (cache-index int) (size vector-2i) (glyph int))

(defgmethod
 (font-file+set-glyph-texture-idx :class 'font-file :bind
  "set_glyph_texture_idx" :hash 355564111)
 :void (cache-index int) (size vector-2i) (glyph int) (texture-idx int))

(defgmethod
 (font-file+get-glyph-texture-idx :class 'font-file :bind
  "get_glyph_texture_idx" :hash 1629411054)
 int (cache-index int) (size vector-2i) (glyph int))

(defgmethod
 (font-file+get-kerning-list :class 'font-file :bind "get_kerning_list" :hash
  2345056839)
 array (cache-index int) (size int))

(defgmethod
 (font-file+clear-kerning-map :class 'font-file :bind "clear_kerning_map" :hash
  3937882851)
 :void (cache-index int) (size int))

(defgmethod
 (font-file+remove-kerning :class 'font-file :bind "remove_kerning" :hash
  3930204747)
 :void (cache-index int) (size int) (glyph-pair vector-2i))

(defgmethod
 (font-file+set-kerning :class 'font-file :bind "set_kerning" :hash 3182200918)
 :void (cache-index int) (size int) (glyph-pair vector-2i) (kerning vector-2))

(defgmethod
 (font-file+get-kerning :class 'font-file :bind "get_kerning" :hash 1611912865)
 vector-2 (cache-index int) (size int) (glyph-pair vector-2i))

(defgmethod
 (font-file+render-range :class 'font-file :bind "render_range" :hash
  355564111)
 :void (cache-index int) (size vector-2i) (start int) (end int))

(defgmethod
 (font-file+render-glyph :class 'font-file :bind "render_glyph" :hash
  2328951467)
 :void (cache-index int) (size vector-2i) (index int))

(defgmethod
 (font-file+set-language-support-override :class 'font-file :bind
  "set_language_support_override" :hash 2678287736)
 :void (language string) (supported bool))

(defgmethod
 (font-file+get-language-support-override :class 'font-file :bind
  "get_language_support_override" :hash 3927539163)
 bool (language string))

(defgmethod
 (font-file+remove-language-support-override :class 'font-file :bind
  "remove_language_support_override" :hash 83702148)
 :void (language string))

(defgmethod
 (font-file+get-language-support-overrides :class 'font-file :bind
  "get_language_support_overrides" :hash 1139954409)
 packed-string-array)

(defgmethod
 (font-file+set-script-support-override :class 'font-file :bind
  "set_script_support_override" :hash 2678287736)
 :void (script string) (supported bool))

(defgmethod
 (font-file+get-script-support-override :class 'font-file :bind
  "get_script_support_override" :hash 3927539163)
 bool (script string))

(defgmethod
 (font-file+remove-script-support-override :class 'font-file :bind
  "remove_script_support_override" :hash 83702148)
 :void (script string))

(defgmethod
 (font-file+get-script-support-overrides :class 'font-file :bind
  "get_script_support_overrides" :hash 1139954409)
 packed-string-array)

(defgmethod
 (font-file+set-opentype-feature-overrides :class 'font-file :bind
  "set_opentype_feature_overrides" :hash 4155329257)
 :void (overrides dictionary))

(defgmethod
 (font-file+get-opentype-feature-overrides :class 'font-file :bind
  "get_opentype_feature_overrides" :hash 3102165223)
 dictionary)

(defgmethod
 (font-file+get-glyph-index :class 'font-file :bind "get_glyph_index" :hash
  864943070)
 int (size int) (char int) (variation-selector int))

(defgmethod
 (font-file+get-char-from-glyph-index :class 'font-file :bind
  "get_char_from_glyph_index" :hash 3175239445)
 int (size int) (glyph-index int))