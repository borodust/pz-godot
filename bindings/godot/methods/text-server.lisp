(common-lisp:in-package :%godot)


(defgmethod
 (text-server+has-feature :class 'text-server :bind "has_feature" :hash
  3967367083)
 bool (feature text-server+feature))

(defgmethod
 (text-server+get-name :class 'text-server :bind "get_name" :hash 201670096)
 string)

(defgmethod
 (text-server+get-features :class 'text-server :bind "get_features" :hash
  3905245786)
 int)

(defgmethod
 (text-server+load-support-data :class 'text-server :bind "load_support_data"
  :hash 2323990056)
 bool (filename string))

(defgmethod
 (text-server+get-support-data-filename :class 'text-server :bind
  "get_support_data_filename" :hash 201670096)
 string)

(defgmethod
 (text-server+get-support-data-info :class 'text-server :bind
  "get_support_data_info" :hash 201670096)
 string)

(defgmethod
 (text-server+save-support-data :class 'text-server :bind "save_support_data"
  :hash 3927539163)
 bool (filename string))

(defgmethod
 (text-server+get-support-data :class 'text-server :bind "get_support_data"
  :hash 2362200018)
 packed-byte-array)

(defgmethod
 (text-server+is-locale-using-support-data :class 'text-server :bind
  "is_locale_using_support_data" :hash 3927539163)
 bool (locale string))

(defgmethod
 (text-server+is-locale-right-to-left :class 'text-server :bind
  "is_locale_right_to_left" :hash 3927539163)
 bool (locale string))

(defgmethod
 (text-server+name-to-tag :class 'text-server :bind "name_to_tag" :hash
  1321353865)
 int (name string))

(defgmethod
 (text-server+tag-to-name :class 'text-server :bind "tag_to_name" :hash
  844755477)
 string (tag int))

(defgmethod (text-server+has :class 'text-server :bind "has" :hash 3521089500)
 bool (rid rid))

(defgmethod
 (text-server+free-rid :class 'text-server :bind "free_rid" :hash 2722037293)
 :void (rid rid))

(defgmethod
 (text-server+create-font :class 'text-server :bind "create_font" :hash
  529393457)
 rid)

(defgmethod
 (text-server+create-font-linked-variation :class 'text-server :bind
  "create_font_linked_variation" :hash 41030802)
 rid (font-rid rid))

(defgmethod
 (text-server+font-set-data :class 'text-server :bind "font_set_data" :hash
  1355495400)
 :void (font-rid rid) (data packed-byte-array))

(defgmethod
 (text-server+font-set-face-index :class 'text-server :bind
  "font_set_face_index" :hash 3411492887)
 :void (font-rid rid) (face-index int))

(defgmethod
 (text-server+font-get-face-index :class 'text-server :bind
  "font_get_face_index" :hash 2198884583)
 int (font-rid rid))

(defgmethod
 (text-server+font-get-face-count :class 'text-server :bind
  "font_get_face_count" :hash 2198884583)
 int (font-rid rid))

(defgmethod
 (text-server+font-set-style :class 'text-server :bind "font_set_style" :hash
  898466325)
 :void (font-rid rid) (style text-server+font-style))

(defgmethod
 (text-server+font-get-style :class 'text-server :bind "font_get_style" :hash
  3082502592)
 text-server+font-style (font-rid rid))

(defgmethod
 (text-server+font-set-name :class 'text-server :bind "font_set_name" :hash
  2726140452)
 :void (font-rid rid) (name string))

(defgmethod
 (text-server+font-get-name :class 'text-server :bind "font_get_name" :hash
  642473191)
 string (font-rid rid))

(defgmethod
 (text-server+font-get-ot-name-strings :class 'text-server :bind
  "font_get_ot_name_strings" :hash 1882737106)
 dictionary (font-rid rid))

(defgmethod
 (text-server+font-set-style-name :class 'text-server :bind
  "font_set_style_name" :hash 2726140452)
 :void (font-rid rid) (name string))

(defgmethod
 (text-server+font-get-style-name :class 'text-server :bind
  "font_get_style_name" :hash 642473191)
 string (font-rid rid))

(defgmethod
 (text-server+font-set-weight :class 'text-server :bind "font_set_weight" :hash
  3411492887)
 :void (font-rid rid) (weight int))

(defgmethod
 (text-server+font-get-weight :class 'text-server :bind "font_get_weight" :hash
  2198884583)
 int (font-rid rid))

(defgmethod
 (text-server+font-set-stretch :class 'text-server :bind "font_set_stretch"
  :hash 3411492887)
 :void (font-rid rid) (weight int))

(defgmethod
 (text-server+font-get-stretch :class 'text-server :bind "font_get_stretch"
  :hash 2198884583)
 int (font-rid rid))

(defgmethod
 (text-server+font-set-antialiasing :class 'text-server :bind
  "font_set_antialiasing" :hash 958337235)
 :void (font-rid rid) (antialiasing text-server+font-antialiasing))

(defgmethod
 (text-server+font-get-antialiasing :class 'text-server :bind
  "font_get_antialiasing" :hash 3389420495)
 text-server+font-antialiasing (font-rid rid))

(defgmethod
 (text-server+font-set-disable-embedded-bitmaps :class 'text-server :bind
  "font_set_disable_embedded_bitmaps" :hash 1265174801)
 :void (font-rid rid) (disable-embedded-bitmaps bool))

(defgmethod
 (text-server+font-get-disable-embedded-bitmaps :class 'text-server :bind
  "font_get_disable_embedded_bitmaps" :hash 4155700596)
 bool (font-rid rid))

(defgmethod
 (text-server+font-set-generate-mipmaps :class 'text-server :bind
  "font_set_generate_mipmaps" :hash 1265174801)
 :void (font-rid rid) (generate-mipmaps bool))

(defgmethod
 (text-server+font-get-generate-mipmaps :class 'text-server :bind
  "font_get_generate_mipmaps" :hash 4155700596)
 bool (font-rid rid))

(defgmethod
 (text-server+font-set-multichannel-signed-distance-field :class 'text-server
  :bind "font_set_multichannel_signed_distance_field" :hash 1265174801)
 :void (font-rid rid) (msdf bool))

(defgmethod
 (text-server+font-is-multichannel-signed-distance-field :class 'text-server
  :bind "font_is_multichannel_signed_distance_field" :hash 4155700596)
 bool (font-rid rid))

(defgmethod
 (text-server+font-set-msdf-pixel-range :class 'text-server :bind
  "font_set_msdf_pixel_range" :hash 3411492887)
 :void (font-rid rid) (msdf-pixel-range int))

(defgmethod
 (text-server+font-get-msdf-pixel-range :class 'text-server :bind
  "font_get_msdf_pixel_range" :hash 2198884583)
 int (font-rid rid))

(defgmethod
 (text-server+font-set-msdf-size :class 'text-server :bind "font_set_msdf_size"
  :hash 3411492887)
 :void (font-rid rid) (msdf-size int))

(defgmethod
 (text-server+font-get-msdf-size :class 'text-server :bind "font_get_msdf_size"
  :hash 2198884583)
 int (font-rid rid))

(defgmethod
 (text-server+font-set-fixed-size :class 'text-server :bind
  "font_set_fixed_size" :hash 3411492887)
 :void (font-rid rid) (fixed-size int))

(defgmethod
 (text-server+font-get-fixed-size :class 'text-server :bind
  "font_get_fixed_size" :hash 2198884583)
 int (font-rid rid))

(defgmethod
 (text-server+font-set-fixed-size-scale-mode :class 'text-server :bind
  "font_set_fixed_size_scale_mode" :hash 1029390307)
 :void (font-rid rid) (fixed-size-scale-mode text-server+fixed-size-scale-mode))

(defgmethod
 (text-server+font-get-fixed-size-scale-mode :class 'text-server :bind
  "font_get_fixed_size_scale_mode" :hash 4113120379)
 text-server+fixed-size-scale-mode (font-rid rid))

(defgmethod
 (text-server+font-set-allow-system-fallback :class 'text-server :bind
  "font_set_allow_system_fallback" :hash 1265174801)
 :void (font-rid rid) (allow-system-fallback bool))

(defgmethod
 (text-server+font-is-allow-system-fallback :class 'text-server :bind
  "font_is_allow_system_fallback" :hash 4155700596)
 bool (font-rid rid))

(defgmethod
 (text-server+font-clear-system-fallback-cache :class 'text-server :bind
  "font_clear_system_fallback_cache" :hash 3218959716)
 :void)

(defgmethod
 (text-server+font-set-force-autohinter :class 'text-server :bind
  "font_set_force_autohinter" :hash 1265174801)
 :void (font-rid rid) (force-autohinter bool))

(defgmethod
 (text-server+font-is-force-autohinter :class 'text-server :bind
  "font_is_force_autohinter" :hash 4155700596)
 bool (font-rid rid))

(defgmethod
 (text-server+font-set-modulate-color-glyphs :class 'text-server :bind
  "font_set_modulate_color_glyphs" :hash 1265174801)
 :void (font-rid rid) (modulate bool))

(defgmethod
 (text-server+font-is-modulate-color-glyphs :class 'text-server :bind
  "font_is_modulate_color_glyphs" :hash 4155700596)
 bool (font-rid rid))

(defgmethod
 (text-server+font-get-palette-count :class 'text-server :bind
  "font_get_palette_count" :hash 2198884583)
 int (font-rid rid))

(defgmethod
 (text-server+font-get-palette-name :class 'text-server :bind
  "font_get_palette_name" :hash 1464764419)
 string (font-rid rid) (index int))

(defgmethod
 (text-server+font-get-palette-colors :class 'text-server :bind
  "font_get_palette_colors" :hash 1595517857)
 packed-color-array (font-rid rid) (index int))

(defgmethod
 (text-server+font-set-palette-custom-colors :class 'text-server :bind
  "font_set_palette_custom_colors" :hash 4037098590)
 :void (font-rid rid) (colors packed-color-array))

(defgmethod
 (text-server+font-get-palette-custom-colors :class 'text-server :bind
  "font_get_palette_custom_colors" :hash 1569415609)
 packed-color-array (font-rid rid))

(defgmethod
 (text-server+font-get-used-palette :class 'text-server :bind
  "font_get_used_palette" :hash 2198884583)
 int (font-rid rid))

(defgmethod
 (text-server+font-set-used-palette :class 'text-server :bind
  "font_set_used_palette" :hash 3411492887)
 :void (font-rid rid) (index int))

(defgmethod
 (text-server+font-set-hinting :class 'text-server :bind "font_set_hinting"
  :hash 1520010864)
 :void (font-rid rid) (hinting text-server+hinting))

(defgmethod
 (text-server+font-get-hinting :class 'text-server :bind "font_get_hinting"
  :hash 3971592737)
 text-server+hinting (font-rid rid))

(defgmethod
 (text-server+font-set-subpixel-positioning :class 'text-server :bind
  "font_set_subpixel_positioning" :hash 3830459669)
 :void (font-rid rid) (subpixel-positioning text-server+subpixel-positioning))

(defgmethod
 (text-server+font-get-subpixel-positioning :class 'text-server :bind
  "font_get_subpixel_positioning" :hash 2752233671)
 text-server+subpixel-positioning (font-rid rid))

(defgmethod
 (text-server+font-set-keep-rounding-remainders :class 'text-server :bind
  "font_set_keep_rounding_remainders" :hash 1265174801)
 :void (font-rid rid) (keep-rounding-remainders bool))

(defgmethod
 (text-server+font-get-keep-rounding-remainders :class 'text-server :bind
  "font_get_keep_rounding_remainders" :hash 4155700596)
 bool (font-rid rid))

(defgmethod
 (text-server+font-set-embolden :class 'text-server :bind "font_set_embolden"
  :hash 1794382983)
 :void (font-rid rid) (strength float))

(defgmethod
 (text-server+font-get-embolden :class 'text-server :bind "font_get_embolden"
  :hash 866169185)
 float (font-rid rid))

(defgmethod
 (text-server+font-set-spacing :class 'text-server :bind "font_set_spacing"
  :hash 1307259930)
 :void (font-rid rid) (spacing text-server+spacing-type) (value int))

(defgmethod
 (text-server+font-get-spacing :class 'text-server :bind "font_get_spacing"
  :hash 1213653558)
 int (font-rid rid) (spacing text-server+spacing-type))

(defgmethod
 (text-server+font-set-baseline-offset :class 'text-server :bind
  "font_set_baseline_offset" :hash 1794382983)
 :void (font-rid rid) (baseline-offset float))

(defgmethod
 (text-server+font-get-baseline-offset :class 'text-server :bind
  "font_get_baseline_offset" :hash 866169185)
 float (font-rid rid))

(defgmethod
 (text-server+font-set-transform :class 'text-server :bind "font_set_transform"
  :hash 1246044741)
 :void (font-rid rid) (transform transform-2d))

(defgmethod
 (text-server+font-get-transform :class 'text-server :bind "font_get_transform"
  :hash 213527486)
 transform-2d (font-rid rid))

(defgmethod
 (text-server+font-set-variation-coordinates :class 'text-server :bind
  "font_set_variation_coordinates" :hash 1217542888)
 :void (font-rid rid) (variation-coordinates dictionary))

(defgmethod
 (text-server+font-get-variation-coordinates :class 'text-server :bind
  "font_get_variation_coordinates" :hash 1882737106)
 dictionary (font-rid rid))

(defgmethod
 (text-server+font-set-oversampling :class 'text-server :bind
  "font_set_oversampling" :hash 1794382983)
 :void (font-rid rid) (oversampling float))

(defgmethod
 (text-server+font-get-oversampling :class 'text-server :bind
  "font_get_oversampling" :hash 866169185)
 float (font-rid rid))

(defgmethod
 (text-server+font-get-size-cache-list :class 'text-server :bind
  "font_get_size_cache_list" :hash 2684255073)
 array (font-rid rid))

(defgmethod
 (text-server+font-clear-size-cache :class 'text-server :bind
  "font_clear_size_cache" :hash 2722037293)
 :void (font-rid rid))

(defgmethod
 (text-server+font-remove-size-cache :class 'text-server :bind
  "font_remove_size_cache" :hash 2450610377)
 :void (font-rid rid) (size vector-2i))

(defgmethod
 (text-server+font-get-size-cache-info :class 'text-server :bind
  "font_get_size_cache_info" :hash 2684255073)
 array (font-rid rid))

(defgmethod
 (text-server+font-set-ascent :class 'text-server :bind "font_set_ascent" :hash
  1892459533)
 :void (font-rid rid) (size int) (ascent float))

(defgmethod
 (text-server+font-get-ascent :class 'text-server :bind "font_get_ascent" :hash
  755457166)
 float (font-rid rid) (size int))

(defgmethod
 (text-server+font-set-descent :class 'text-server :bind "font_set_descent"
  :hash 1892459533)
 :void (font-rid rid) (size int) (descent float))

(defgmethod
 (text-server+font-get-descent :class 'text-server :bind "font_get_descent"
  :hash 755457166)
 float (font-rid rid) (size int))

(defgmethod
 (text-server+font-set-underline-position :class 'text-server :bind
  "font_set_underline_position" :hash 1892459533)
 :void (font-rid rid) (size int) (underline-position float))

(defgmethod
 (text-server+font-get-underline-position :class 'text-server :bind
  "font_get_underline_position" :hash 755457166)
 float (font-rid rid) (size int))

(defgmethod
 (text-server+font-set-underline-thickness :class 'text-server :bind
  "font_set_underline_thickness" :hash 1892459533)
 :void (font-rid rid) (size int) (underline-thickness float))

(defgmethod
 (text-server+font-get-underline-thickness :class 'text-server :bind
  "font_get_underline_thickness" :hash 755457166)
 float (font-rid rid) (size int))

(defgmethod
 (text-server+font-set-scale :class 'text-server :bind "font_set_scale" :hash
  1892459533)
 :void (font-rid rid) (size int) (scale float))

(defgmethod
 (text-server+font-get-scale :class 'text-server :bind "font_get_scale" :hash
  755457166)
 float (font-rid rid) (size int))

(defgmethod
 (text-server+font-get-texture-count :class 'text-server :bind
  "font_get_texture_count" :hash 1311001310)
 int (font-rid rid) (size vector-2i))

(defgmethod
 (text-server+font-clear-textures :class 'text-server :bind
  "font_clear_textures" :hash 2450610377)
 :void (font-rid rid) (size vector-2i))

(defgmethod
 (text-server+font-remove-texture :class 'text-server :bind
  "font_remove_texture" :hash 3810512262)
 :void (font-rid rid) (size vector-2i) (texture-index int))

(defgmethod
 (text-server+font-set-texture-image :class 'text-server :bind
  "font_set_texture_image" :hash 2354485091)
 :void (font-rid rid) (size vector-2i) (texture-index int) (image image))

(defgmethod
 (text-server+font-get-texture-image :class 'text-server :bind
  "font_get_texture_image" :hash 2451761155)
 image (font-rid rid) (size vector-2i) (texture-index int))

(defgmethod
 (text-server+font-set-texture-offsets :class 'text-server :bind
  "font_set_texture_offsets" :hash 3005398047)
 :void (font-rid rid) (size vector-2i) (texture-index int)
 (offset packed-int-32array))

(defgmethod
 (text-server+font-get-texture-offsets :class 'text-server :bind
  "font_get_texture_offsets" :hash 3420028887)
 packed-int-32array (font-rid rid) (size vector-2i) (texture-index int))

(defgmethod
 (text-server+font-get-glyph-list :class 'text-server :bind
  "font_get_glyph_list" :hash 46086620)
 packed-int-32array (font-rid rid) (size vector-2i))

(defgmethod
 (text-server+font-clear-glyphs :class 'text-server :bind "font_clear_glyphs"
  :hash 2450610377)
 :void (font-rid rid) (size vector-2i))

(defgmethod
 (text-server+font-remove-glyph :class 'text-server :bind "font_remove_glyph"
  :hash 3810512262)
 :void (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server+font-get-glyph-advance :class 'text-server :bind
  "font_get_glyph_advance" :hash 2555689501)
 vector-2 (font-rid rid) (size int) (glyph int))

(defgmethod
 (text-server+font-set-glyph-advance :class 'text-server :bind
  "font_set_glyph_advance" :hash 3219397315)
 :void (font-rid rid) (size int) (glyph int) (advance vector-2))

(defgmethod
 (text-server+font-get-glyph-offset :class 'text-server :bind
  "font_get_glyph_offset" :hash 513728628)
 vector-2 (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server+font-set-glyph-offset :class 'text-server :bind
  "font_set_glyph_offset" :hash 1812632090)
 :void (font-rid rid) (size vector-2i) (glyph int) (offset vector-2))

(defgmethod
 (text-server+font-get-glyph-size :class 'text-server :bind
  "font_get_glyph_size" :hash 513728628)
 vector-2 (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server+font-set-glyph-size :class 'text-server :bind
  "font_set_glyph_size" :hash 1812632090)
 :void (font-rid rid) (size vector-2i) (glyph int) (gl-size vector-2))

(defgmethod
 (text-server+font-get-glyph-uv-rect :class 'text-server :bind
  "font_get_glyph_uv_rect" :hash 2274268786)
 rect-2 (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server+font-set-glyph-uv-rect :class 'text-server :bind
  "font_set_glyph_uv_rect" :hash 1973324081)
 :void (font-rid rid) (size vector-2i) (glyph int) (uv-rect rect-2))

(defgmethod
 (text-server+font-get-glyph-texture-idx :class 'text-server :bind
  "font_get_glyph_texture_idx" :hash 4292800474)
 int (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server+font-set-glyph-texture-idx :class 'text-server :bind
  "font_set_glyph_texture_idx" :hash 4254580980)
 :void (font-rid rid) (size vector-2i) (glyph int) (texture-idx int))

(defgmethod
 (text-server+font-get-glyph-texture-rid :class 'text-server :bind
  "font_get_glyph_texture_rid" :hash 1451696141)
 rid (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server+font-get-glyph-texture-size :class 'text-server :bind
  "font_get_glyph_texture_size" :hash 513728628)
 vector-2 (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server+font-get-glyph-contours :class 'text-server :bind
  "font_get_glyph_contours" :hash 2903964473)
 dictionary (font rid) (size int) (index int))

(defgmethod
 (text-server+font-get-kerning-list :class 'text-server :bind
  "font_get_kerning_list" :hash 1778388067)
 array (font-rid rid) (size int))

(defgmethod
 (text-server+font-clear-kerning-map :class 'text-server :bind
  "font_clear_kerning_map" :hash 3411492887)
 :void (font-rid rid) (size int))

(defgmethod
 (text-server+font-remove-kerning :class 'text-server :bind
  "font_remove_kerning" :hash 2141860016)
 :void (font-rid rid) (size int) (glyph-pair vector-2i))

(defgmethod
 (text-server+font-set-kerning :class 'text-server :bind "font_set_kerning"
  :hash 3630965883)
 :void (font-rid rid) (size int) (glyph-pair vector-2i) (kerning vector-2))

(defgmethod
 (text-server+font-get-kerning :class 'text-server :bind "font_get_kerning"
  :hash 1019980169)
 vector-2 (font-rid rid) (size int) (glyph-pair vector-2i))

(defgmethod
 (text-server+font-get-glyph-index :class 'text-server :bind
  "font_get_glyph_index" :hash 1765635060)
 int (font-rid rid) (size int) (char int) (variation-selector int))

(defgmethod
 (text-server+font-get-char-from-glyph-index :class 'text-server :bind
  "font_get_char_from_glyph_index" :hash 2156738276)
 int (font-rid rid) (size int) (glyph-index int))

(defgmethod
 (text-server+font-has-char :class 'text-server :bind "font_has_char" :hash
  3120086654)
 bool (font-rid rid) (char int))

(defgmethod
 (text-server+font-get-supported-chars :class 'text-server :bind
  "font_get_supported_chars" :hash 642473191)
 string (font-rid rid))

(defgmethod
 (text-server+font-get-supported-glyphs :class 'text-server :bind
  "font_get_supported_glyphs" :hash 788230395)
 packed-int-32array (font-rid rid))

(defgmethod
 (text-server+font-render-range :class 'text-server :bind "font_render_range"
  :hash 4254580980)
 :void (font-rid rid) (size vector-2i) (start int) (end int))

(defgmethod
 (text-server+font-render-glyph :class 'text-server :bind "font_render_glyph"
  :hash 3810512262)
 :void (font-rid rid) (size vector-2i) (index int))

(defgmethod
 (text-server+font-draw-glyph :class 'text-server :bind "font_draw_glyph" :hash
  3103234926)
 :void (font-rid rid) (canvas rid) (size int) (pos vector-2) (index int)
 (color color) (oversampling float))

(defgmethod
 (text-server+font-draw-glyph-outline :class 'text-server :bind
  "font_draw_glyph_outline" :hash 1976041553)
 :void (font-rid rid) (canvas rid) (size int) (outline-size int) (pos vector-2)
 (index int) (color color) (oversampling float))

(defgmethod
 (text-server+font-is-language-supported :class 'text-server :bind
  "font_is_language_supported" :hash 3199320846)
 bool (font-rid rid) (language string))

(defgmethod
 (text-server+font-set-language-support-override :class 'text-server :bind
  "font_set_language_support_override" :hash 2313957094)
 :void (font-rid rid) (language string) (supported bool))

(defgmethod
 (text-server+font-get-language-support-override :class 'text-server :bind
  "font_get_language_support_override" :hash 2829184646)
 bool (font-rid rid) (language string))

(defgmethod
 (text-server+font-remove-language-support-override :class 'text-server :bind
  "font_remove_language_support_override" :hash 2726140452)
 :void (font-rid rid) (language string))

(defgmethod
 (text-server+font-get-language-support-overrides :class 'text-server :bind
  "font_get_language_support_overrides" :hash 2801473409)
 packed-string-array (font-rid rid))

(defgmethod
 (text-server+font-is-script-supported :class 'text-server :bind
  "font_is_script_supported" :hash 3199320846)
 bool (font-rid rid) (script string))

(defgmethod
 (text-server+font-set-script-support-override :class 'text-server :bind
  "font_set_script_support_override" :hash 2313957094)
 :void (font-rid rid) (script string) (supported bool))

(defgmethod
 (text-server+font-get-script-support-override :class 'text-server :bind
  "font_get_script_support_override" :hash 2829184646)
 bool (font-rid rid) (script string))

(defgmethod
 (text-server+font-remove-script-support-override :class 'text-server :bind
  "font_remove_script_support_override" :hash 2726140452)
 :void (font-rid rid) (script string))

(defgmethod
 (text-server+font-get-script-support-overrides :class 'text-server :bind
  "font_get_script_support_overrides" :hash 2801473409)
 packed-string-array (font-rid rid))

(defgmethod
 (text-server+font-set-opentype-feature-overrides :class 'text-server :bind
  "font_set_opentype_feature_overrides" :hash 1217542888)
 :void (font-rid rid) (overrides dictionary))

(defgmethod
 (text-server+font-get-opentype-feature-overrides :class 'text-server :bind
  "font_get_opentype_feature_overrides" :hash 1882737106)
 dictionary (font-rid rid))

(defgmethod
 (text-server+font-supported-feature-list :class 'text-server :bind
  "font_supported_feature_list" :hash 1882737106)
 dictionary (font-rid rid))

(defgmethod
 (text-server+font-supported-variation-list :class 'text-server :bind
  "font_supported_variation_list" :hash 1882737106)
 dictionary (font-rid rid))

(defgmethod
 (text-server+font-get-global-oversampling :class 'text-server :bind
  "font_get_global_oversampling" :hash 1740695150)
 float)

(defgmethod
 (text-server+font-set-global-oversampling :class 'text-server :bind
  "font_set_global_oversampling" :hash 373806689)
 :void (oversampling float))

(defgmethod
 (text-server+get-hex-code-box-size :class 'text-server :bind
  "get_hex_code_box_size" :hash 3016396712)
 vector-2 (size int) (index int))

(defgmethod
 (text-server+draw-hex-code-box :class 'text-server :bind "draw_hex_code_box"
  :hash 1602046441)
 :void (canvas rid) (size int) (pos vector-2) (index int) (color color))

(defgmethod
 (text-server+create-shaped-text :class 'text-server :bind "create_shaped_text"
  :hash 1231398698)
 rid (direction text-server+direction) (orientation text-server+orientation))

(defgmethod
 (text-server+shaped-text-clear :class 'text-server :bind "shaped_text_clear"
  :hash 2722037293)
 :void (rid rid))

(defgmethod
 (text-server+shaped-text-duplicate :class 'text-server :bind
  "shaped_text_duplicate" :hash 41030802)
 rid (rid rid))

(defgmethod
 (text-server+shaped-text-set-direction :class 'text-server :bind
  "shaped_text_set_direction" :hash 1551430183)
 :void (shaped rid) (direction text-server+direction))

(defgmethod
 (text-server+shaped-text-get-direction :class 'text-server :bind
  "shaped_text_get_direction" :hash 3065904362)
 text-server+direction (shaped rid))

(defgmethod
 (text-server+shaped-text-get-inferred-direction :class 'text-server :bind
  "shaped_text_get_inferred_direction" :hash 3065904362)
 text-server+direction (shaped rid))

(defgmethod
 (text-server+shaped-text-set-bidi-override :class 'text-server :bind
  "shaped_text_set_bidi_override" :hash 684822712)
 :void (shaped rid) (override array))

(defgmethod
 (text-server+shaped-text-set-custom-punctuation :class 'text-server :bind
  "shaped_text_set_custom_punctuation" :hash 2726140452)
 :void (shaped rid) (punct string))

(defgmethod
 (text-server+shaped-text-get-custom-punctuation :class 'text-server :bind
  "shaped_text_get_custom_punctuation" :hash 642473191)
 string (shaped rid))

(defgmethod
 (text-server+shaped-text-set-custom-ellipsis :class 'text-server :bind
  "shaped_text_set_custom_ellipsis" :hash 3411492887)
 :void (shaped rid) (char int))

(defgmethod
 (text-server+shaped-text-get-custom-ellipsis :class 'text-server :bind
  "shaped_text_get_custom_ellipsis" :hash 2198884583)
 int (shaped rid))

(defgmethod
 (text-server+shaped-text-set-orientation :class 'text-server :bind
  "shaped_text_set_orientation" :hash 3019609126)
 :void (shaped rid) (orientation text-server+orientation))

(defgmethod
 (text-server+shaped-text-get-orientation :class 'text-server :bind
  "shaped_text_get_orientation" :hash 3142708106)
 text-server+orientation (shaped rid))

(defgmethod
 (text-server+shaped-text-set-preserve-invalid :class 'text-server :bind
  "shaped_text_set_preserve_invalid" :hash 1265174801)
 :void (shaped rid) (enabled bool))

(defgmethod
 (text-server+shaped-text-get-preserve-invalid :class 'text-server :bind
  "shaped_text_get_preserve_invalid" :hash 4155700596)
 bool (shaped rid))

(defgmethod
 (text-server+shaped-text-set-preserve-control :class 'text-server :bind
  "shaped_text_set_preserve_control" :hash 1265174801)
 :void (shaped rid) (enabled bool))

(defgmethod
 (text-server+shaped-text-get-preserve-control :class 'text-server :bind
  "shaped_text_get_preserve_control" :hash 4155700596)
 bool (shaped rid))

(defgmethod
 (text-server+shaped-text-set-spacing :class 'text-server :bind
  "shaped_text_set_spacing" :hash 1307259930)
 :void (shaped rid) (spacing text-server+spacing-type) (value int))

(defgmethod
 (text-server+shaped-text-get-spacing :class 'text-server :bind
  "shaped_text_get_spacing" :hash 1213653558)
 int (shaped rid) (spacing text-server+spacing-type))

(defgmethod
 (text-server+shaped-text-add-string :class 'text-server :bind
  "shaped_text_add_string" :hash 623473029)
 bool (shaped rid) (text string) (fonts array) (size int)
 (opentype-features dictionary) (language string) (meta variant))

(defgmethod
 (text-server+shaped-text-add-object :class 'text-server :bind
  "shaped_text_add_object" :hash 3664424789)
 bool (shaped rid) (key variant) (size vector-2)
 (inline-align inline-alignment) (length int) (baseline float))

(defgmethod
 (text-server+shaped-text-resize-object :class 'text-server :bind
  "shaped_text_resize_object" :hash 790361552)
 bool (shaped rid) (key variant) (size vector-2)
 (inline-align inline-alignment) (baseline float))

(defgmethod
 (text-server+shaped-text-has-object :class 'text-server :bind
  "shaped_text_has_object" :hash 2360964694)
 bool (shaped rid) (key variant))

(defgmethod
 (text-server+shaped-get-text :class 'text-server :bind "shaped_get_text" :hash
  642473191)
 string (shaped rid))

(defgmethod
 (text-server+shaped-get-span-count :class 'text-server :bind
  "shaped_get_span_count" :hash 2198884583)
 int (shaped rid))

(defgmethod
 (text-server+shaped-get-span-meta :class 'text-server :bind
  "shaped_get_span_meta" :hash 4069510997)
 variant (shaped rid) (index int))

(defgmethod
 (text-server+shaped-get-span-embedded-object :class 'text-server :bind
  "shaped_get_span_embedded_object" :hash 4069510997)
 variant (shaped rid) (index int))

(defgmethod
 (text-server+shaped-get-span-text :class 'text-server :bind
  "shaped_get_span_text" :hash 1464764419)
 string (shaped rid) (index int))

(defgmethod
 (text-server+shaped-get-span-object :class 'text-server :bind
  "shaped_get_span_object" :hash 4069510997)
 variant (shaped rid) (index int))

(defgmethod
 (text-server+shaped-set-span-update-font :class 'text-server :bind
  "shaped_set_span_update_font" :hash 2022725822)
 :void (shaped rid) (index int) (fonts array) (size int)
 (opentype-features dictionary))

(defgmethod
 (text-server+shaped-get-run-count :class 'text-server :bind
  "shaped_get_run_count" :hash 2198884583)
 int (shaped rid))

(defgmethod
 (text-server+shaped-get-run-text :class 'text-server :bind
  "shaped_get_run_text" :hash 1464764419)
 string (shaped rid) (index int))

(defgmethod
 (text-server+shaped-get-run-range :class 'text-server :bind
  "shaped_get_run_range" :hash 4069534484)
 vector-2i (shaped rid) (index int))

(defgmethod
 (text-server+shaped-get-run-glyph-range :class 'text-server :bind
  "shaped_get_run_glyph_range" :hash 4069534484)
 vector-2i (shaped rid) (index int))

(defgmethod
 (text-server+shaped-get-run-font-rid :class 'text-server :bind
  "shaped_get_run_font_rid" :hash 1066463050)
 rid (shaped rid) (index int))

(defgmethod
 (text-server+shaped-get-run-font-size :class 'text-server :bind
  "shaped_get_run_font_size" :hash 1120910005)
 int (shaped rid) (index int))

(defgmethod
 (text-server+shaped-get-run-language :class 'text-server :bind
  "shaped_get_run_language" :hash 1464764419)
 string (shaped rid) (index int))

(defgmethod
 (text-server+shaped-get-run-direction :class 'text-server :bind
  "shaped_get_run_direction" :hash 2413896864)
 text-server+direction (shaped rid) (index int))

(defgmethod
 (text-server+shaped-get-run-object :class 'text-server :bind
  "shaped_get_run_object" :hash 4069510997)
 variant (shaped rid) (index int))

(defgmethod
 (text-server+shaped-text-substr :class 'text-server :bind "shaped_text_substr"
  :hash 1937682086)
 rid (shaped rid) (start int) (length int))

(defgmethod
 (text-server+shaped-text-get-parent :class 'text-server :bind
  "shaped_text_get_parent" :hash 3814569979)
 rid (shaped rid))

(defgmethod
 (text-server+shaped-text-fit-to-width :class 'text-server :bind
  "shaped_text_fit_to_width" :hash 530670926)
 float (shaped rid) (width float)
 (justification-flags text-server+justification-flag))

(defgmethod
 (text-server+shaped-text-tab-align :class 'text-server :bind
  "shaped_text_tab_align" :hash 1283669550)
 float (shaped rid) (tab-stops packed-float-32array))

(defgmethod
 (text-server+shaped-text-shape :class 'text-server :bind "shaped_text_shape"
  :hash 3521089500)
 bool (shaped rid))

(defgmethod
 (text-server+shaped-text-is-ready :class 'text-server :bind
  "shaped_text_is_ready" :hash 4155700596)
 bool (shaped rid))

(defgmethod
 (text-server+shaped-text-has-visible-chars :class 'text-server :bind
  "shaped_text_has_visible_chars" :hash 4155700596)
 bool (shaped rid))

(defgmethod
 (text-server+shaped-text-get-glyphs :class 'text-server :bind
  "shaped_text_get_glyphs" :hash 2684255073)
 array (shaped rid))

(defgmethod
 (text-server+shaped-text-sort-logical :class 'text-server :bind
  "shaped_text_sort_logical" :hash 2670461153)
 array (shaped rid))

(defgmethod
 (text-server+shaped-text-get-glyph-count :class 'text-server :bind
  "shaped_text_get_glyph_count" :hash 2198884583)
 int (shaped rid))

(defgmethod
 (text-server+shaped-text-get-range :class 'text-server :bind
  "shaped_text_get_range" :hash 733700038)
 vector-2i (shaped rid))

(defgmethod
 (text-server+shaped-text-get-line-breaks-adv :class 'text-server :bind
  "shaped_text_get_line_breaks_adv" :hash 2376991424)
 packed-int-32array (shaped rid) (width packed-float-32array) (start int)
 (once bool) (break-flags text-server+line-break-flag))

(defgmethod
 (text-server+shaped-text-get-line-breaks :class 'text-server :bind
  "shaped_text_get_line_breaks" :hash 2651359741)
 packed-int-32array (shaped rid) (width float) (start int)
 (break-flags text-server+line-break-flag))

(defgmethod
 (text-server+shaped-text-get-word-breaks :class 'text-server :bind
  "shaped_text_get_word_breaks" :hash 4099476853)
 packed-int-32array (shaped rid) (grapheme-flags text-server+grapheme-flag)
 (skip-grapheme-flags text-server+grapheme-flag))

(defgmethod
 (text-server+shaped-text-get-trim-pos :class 'text-server :bind
  "shaped_text_get_trim_pos" :hash 2198884583)
 int (shaped rid))

(defgmethod
 (text-server+shaped-text-get-ellipsis-pos :class 'text-server :bind
  "shaped_text_get_ellipsis_pos" :hash 2198884583)
 int (shaped rid))

(defgmethod
 (text-server+shaped-text-get-ellipsis-glyphs :class 'text-server :bind
  "shaped_text_get_ellipsis_glyphs" :hash 2684255073)
 array (shaped rid))

(defgmethod
 (text-server+shaped-text-get-ellipsis-glyph-count :class 'text-server :bind
  "shaped_text_get_ellipsis_glyph_count" :hash 2198884583)
 int (shaped rid))

(defgmethod
 (text-server+shaped-text-overrun-trim-to-width :class 'text-server :bind
  "shaped_text_overrun_trim_to_width" :hash 2723146520)
 :void (shaped rid) (width float)
 (overrun-trim-flags text-server+text-overrun-flag))

(defgmethod
 (text-server+shaped-text-get-objects :class 'text-server :bind
  "shaped_text_get_objects" :hash 2684255073)
 array (shaped rid))

(defgmethod
 (text-server+shaped-text-get-object-rect :class 'text-server :bind
  "shaped_text_get_object_rect" :hash 447978354)
 rect-2 (shaped rid) (key variant))

(defgmethod
 (text-server+shaped-text-get-object-range :class 'text-server :bind
  "shaped_text_get_object_range" :hash 2524675647)
 vector-2i (shaped rid) (key variant))

(defgmethod
 (text-server+shaped-text-get-object-glyph :class 'text-server :bind
  "shaped_text_get_object_glyph" :hash 1260085030)
 int (shaped rid) (key variant))

(defgmethod
 (text-server+shaped-text-get-size :class 'text-server :bind
  "shaped_text_get_size" :hash 2440833711)
 vector-2 (shaped rid))

(defgmethod
 (text-server+shaped-text-get-ascent :class 'text-server :bind
  "shaped_text_get_ascent" :hash 866169185)
 float (shaped rid))

(defgmethod
 (text-server+shaped-text-get-descent :class 'text-server :bind
  "shaped_text_get_descent" :hash 866169185)
 float (shaped rid))

(defgmethod
 (text-server+shaped-text-get-width :class 'text-server :bind
  "shaped_text_get_width" :hash 866169185)
 float (shaped rid))

(defgmethod
 (text-server+shaped-text-get-underline-position :class 'text-server :bind
  "shaped_text_get_underline_position" :hash 866169185)
 float (shaped rid))

(defgmethod
 (text-server+shaped-text-get-underline-thickness :class 'text-server :bind
  "shaped_text_get_underline_thickness" :hash 866169185)
 float (shaped rid))

(defgmethod
 (text-server+shaped-text-get-carets :class 'text-server :bind
  "shaped_text_get_carets" :hash 1574219346)
 dictionary (shaped rid) (position int))

(defgmethod
 (text-server+shaped-text-get-selection :class 'text-server :bind
  "shaped_text_get_selection" :hash 3714187733)
 packed-vector-2array (shaped rid) (start int) (end int))

(defgmethod
 (text-server+shaped-text-hit-test-grapheme :class 'text-server :bind
  "shaped_text_hit_test_grapheme" :hash 3149310417)
 int (shaped rid) (coords float))

(defgmethod
 (text-server+shaped-text-hit-test-position :class 'text-server :bind
  "shaped_text_hit_test_position" :hash 3149310417)
 int (shaped rid) (coords float))

(defgmethod
 (text-server+shaped-text-get-grapheme-bounds :class 'text-server :bind
  "shaped_text_get_grapheme_bounds" :hash 2546185844)
 vector-2 (shaped rid) (pos int))

(defgmethod
 (text-server+shaped-text-next-grapheme-pos :class 'text-server :bind
  "shaped_text_next_grapheme_pos" :hash 1120910005)
 int (shaped rid) (pos int))

(defgmethod
 (text-server+shaped-text-prev-grapheme-pos :class 'text-server :bind
  "shaped_text_prev_grapheme_pos" :hash 1120910005)
 int (shaped rid) (pos int))

(defgmethod
 (text-server+shaped-text-get-character-breaks :class 'text-server :bind
  "shaped_text_get_character_breaks" :hash 788230395)
 packed-int-32array (shaped rid))

(defgmethod
 (text-server+shaped-text-next-character-pos :class 'text-server :bind
  "shaped_text_next_character_pos" :hash 1120910005)
 int (shaped rid) (pos int))

(defgmethod
 (text-server+shaped-text-prev-character-pos :class 'text-server :bind
  "shaped_text_prev_character_pos" :hash 1120910005)
 int (shaped rid) (pos int))

(defgmethod
 (text-server+shaped-text-closest-character-pos :class 'text-server :bind
  "shaped_text_closest_character_pos" :hash 1120910005)
 int (shaped rid) (pos int))

(defgmethod
 (text-server+shaped-text-draw :class 'text-server :bind "shaped_text_draw"
  :hash 1647687596)
 :void (shaped rid) (canvas rid) (pos vector-2) (clip-l float) (clip-r float)
 (color color) (oversampling float))

(defgmethod
 (text-server+shaped-text-draw-outline :class 'text-server :bind
  "shaped_text_draw_outline" :hash 1217146601)
 :void (shaped rid) (canvas rid) (pos vector-2) (clip-l float) (clip-r float)
 (outline-size int) (color color) (oversampling float))

(defgmethod
 (text-server+shaped-text-get-dominant-direction-in-range :class 'text-server
  :bind "shaped_text_get_dominant_direction_in_range" :hash 3326907668)
 text-server+direction (shaped rid) (start int) (end int))

(defgmethod
 (text-server+format-number :class 'text-server :bind "format_number" :hash
  2664628024)
 string (number string) (language string))

(defgmethod
 (text-server+parse-number :class 'text-server :bind "parse_number" :hash
  2664628024)
 string (number string) (language string))

(defgmethod
 (text-server+percent-sign :class 'text-server :bind "percent_sign" :hash
  993269549)
 string (language string))

(defgmethod
 (text-server+string-get-word-breaks :class 'text-server :bind
  "string_get_word_breaks" :hash 581857818)
 packed-int-32array (string string) (language string) (chars-per-line int))

(defgmethod
 (text-server+string-get-character-breaks :class 'text-server :bind
  "string_get_character_breaks" :hash 2333794773)
 packed-int-32array (string string) (language string))

(defgmethod
 (text-server+is-confusable :class 'text-server :bind "is_confusable" :hash
  1433197768)
 int (string string) (dict packed-string-array))

(defgmethod
 (text-server+spoof-check :class 'text-server :bind "spoof_check" :hash
  3927539163)
 bool (string string))

(defgmethod
 (text-server+strip-diacritics :class 'text-server :bind "strip_diacritics"
  :hash 3135753539)
 string (string string))

(defgmethod
 (text-server+is-valid-identifier :class 'text-server :bind
  "is_valid_identifier" :hash 3927539163)
 bool (string string))

(defgmethod
 (text-server+is-valid-letter :class 'text-server :bind "is_valid_letter" :hash
  1116898809)
 bool (unicode int))

(defgmethod
 (text-server+string-to-upper :class 'text-server :bind "string_to_upper" :hash
  2664628024)
 string (string string) (language string))

(defgmethod
 (text-server+string-to-lower :class 'text-server :bind "string_to_lower" :hash
  2664628024)
 string (string string) (language string))

(defgmethod
 (text-server+string-to-title :class 'text-server :bind "string_to_title" :hash
  2664628024)
 string (string string) (language string))

(defgmethod
 (text-server+parse-structured-text :class 'text-server :bind
  "parse_structured_text" :hash 3310685015)
 array (parser-type text-server+structured-text-parser) (args array)
 (text string))