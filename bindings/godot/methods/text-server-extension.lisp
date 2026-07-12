(common-lisp:in-package :%godot)


(defgmethod
 (text-server-extension+%has-feature :class 'text-server-extension :bind
  "_has_feature" :hash 3967367083 :virtual common-lisp:t)
 bool (feature text-server+feature))

(defgmethod
 (text-server-extension+%get-name :class 'text-server-extension :bind
  "_get_name" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (text-server-extension+%get-features :class 'text-server-extension :bind
  "_get_features" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (text-server-extension+%free-rid :class 'text-server-extension :bind
  "_free_rid" :hash 2722037293 :virtual common-lisp:t)
 :void (rid rid))

(defgmethod
 (text-server-extension+%has :class 'text-server-extension :bind "_has" :hash
  3521089500 :virtual common-lisp:t)
 bool (rid rid))

(defgmethod
 (text-server-extension+%load-support-data :class 'text-server-extension :bind
  "_load_support_data" :hash 2323990056 :virtual common-lisp:t)
 bool (filename string))

(defgmethod
 (text-server-extension+%get-support-data-filename :class
  'text-server-extension :bind "_get_support_data_filename" :hash 201670096
  :virtual common-lisp:t)
 string)

(defgmethod
 (text-server-extension+%get-support-data-info :class 'text-server-extension
  :bind "_get_support_data_info" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (text-server-extension+%save-support-data :class 'text-server-extension :bind
  "_save_support_data" :hash 3927539163 :virtual common-lisp:t)
 bool (filename string))

(defgmethod
 (text-server-extension+%get-support-data :class 'text-server-extension :bind
  "_get_support_data" :hash 2362200018 :virtual common-lisp:t)
 packed-byte-array)

(defgmethod
 (text-server-extension+%is-locale-using-support-data :class
  'text-server-extension :bind "_is_locale_using_support_data" :hash 3927539163
  :virtual common-lisp:t)
 bool (locale string))

(defgmethod
 (text-server-extension+%is-locale-right-to-left :class 'text-server-extension
  :bind "_is_locale_right_to_left" :hash 3927539163 :virtual common-lisp:t)
 bool (locale string))

(defgmethod
 (text-server-extension+%name-to-tag :class 'text-server-extension :bind
  "_name_to_tag" :hash 1321353865 :virtual common-lisp:t)
 int (name string))

(defgmethod
 (text-server-extension+%tag-to-name :class 'text-server-extension :bind
  "_tag_to_name" :hash 844755477 :virtual common-lisp:t)
 string (tag int))

(defgmethod
 (text-server-extension+%create-font :class 'text-server-extension :bind
  "_create_font" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (text-server-extension+%create-font-linked-variation :class
  'text-server-extension :bind "_create_font_linked_variation" :hash 41030802
  :virtual common-lisp:t)
 rid (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-data :class 'text-server-extension :bind
  "_font_set_data" :hash 1355495400 :virtual common-lisp:t)
 :void (font-rid rid) (data packed-byte-array))

(defgmethod
 (text-server-extension+%font-set-data-ptr :class 'text-server-extension :bind
  "_font_set_data_ptr" :hash 4288446313 :virtual common-lisp:t)
 :void (font-rid rid) (data-ptr (:pointer :uint8)) (data-size int))

(defgmethod
 (text-server-extension+%font-set-face-index :class 'text-server-extension
  :bind "_font_set_face_index" :hash 3411492887 :virtual common-lisp:t)
 :void (font-rid rid) (face-index int))

(defgmethod
 (text-server-extension+%font-get-face-index :class 'text-server-extension
  :bind "_font_get_face_index" :hash 2198884583 :virtual common-lisp:t)
 int (font-rid rid))

(defgmethod
 (text-server-extension+%font-get-face-count :class 'text-server-extension
  :bind "_font_get_face_count" :hash 2198884583 :virtual common-lisp:t)
 int (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-style :class 'text-server-extension :bind
  "_font_set_style" :hash 898466325 :virtual common-lisp:t)
 :void (font-rid rid) (style text-server+font-style))

(defgmethod
 (text-server-extension+%font-get-style :class 'text-server-extension :bind
  "_font_get_style" :hash 3082502592 :virtual common-lisp:t)
 text-server+font-style (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-name :class 'text-server-extension :bind
  "_font_set_name" :hash 2726140452 :virtual common-lisp:t)
 :void (font-rid rid) (name string))

(defgmethod
 (text-server-extension+%font-get-name :class 'text-server-extension :bind
  "_font_get_name" :hash 642473191 :virtual common-lisp:t)
 string (font-rid rid))

(defgmethod
 (text-server-extension+%font-get-ot-name-strings :class 'text-server-extension
  :bind "_font_get_ot_name_strings" :hash 1882737106 :virtual common-lisp:t)
 dictionary (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-style-name :class 'text-server-extension
  :bind "_font_set_style_name" :hash 2726140452 :virtual common-lisp:t)
 :void (font-rid rid) (name-style string))

(defgmethod
 (text-server-extension+%font-get-style-name :class 'text-server-extension
  :bind "_font_get_style_name" :hash 642473191 :virtual common-lisp:t)
 string (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-weight :class 'text-server-extension :bind
  "_font_set_weight" :hash 3411492887 :virtual common-lisp:t)
 :void (font-rid rid) (weight int))

(defgmethod
 (text-server-extension+%font-get-weight :class 'text-server-extension :bind
  "_font_get_weight" :hash 2198884583 :virtual common-lisp:t)
 int (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-stretch :class 'text-server-extension :bind
  "_font_set_stretch" :hash 3411492887 :virtual common-lisp:t)
 :void (font-rid rid) (stretch int))

(defgmethod
 (text-server-extension+%font-get-stretch :class 'text-server-extension :bind
  "_font_get_stretch" :hash 2198884583 :virtual common-lisp:t)
 int (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-antialiasing :class 'text-server-extension
  :bind "_font_set_antialiasing" :hash 958337235 :virtual common-lisp:t)
 :void (font-rid rid) (antialiasing text-server+font-antialiasing))

(defgmethod
 (text-server-extension+%font-get-antialiasing :class 'text-server-extension
  :bind "_font_get_antialiasing" :hash 3389420495 :virtual common-lisp:t)
 text-server+font-antialiasing (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-disable-embedded-bitmaps :class
  'text-server-extension :bind "_font_set_disable_embedded_bitmaps" :hash
  1265174801 :virtual common-lisp:t)
 :void (font-rid rid) (disable-embedded-bitmaps bool))

(defgmethod
 (text-server-extension+%font-get-disable-embedded-bitmaps :class
  'text-server-extension :bind "_font_get_disable_embedded_bitmaps" :hash
  4155700596 :virtual common-lisp:t)
 bool (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-generate-mipmaps :class
  'text-server-extension :bind "_font_set_generate_mipmaps" :hash 1265174801
  :virtual common-lisp:t)
 :void (font-rid rid) (generate-mipmaps bool))

(defgmethod
 (text-server-extension+%font-get-generate-mipmaps :class
  'text-server-extension :bind "_font_get_generate_mipmaps" :hash 4155700596
  :virtual common-lisp:t)
 bool (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-multichannel-signed-distance-field :class
  'text-server-extension :bind "_font_set_multichannel_signed_distance_field"
  :hash 1265174801 :virtual common-lisp:t)
 :void (font-rid rid) (msdf bool))

(defgmethod
 (text-server-extension+%font-is-multichannel-signed-distance-field :class
  'text-server-extension :bind "_font_is_multichannel_signed_distance_field"
  :hash 4155700596 :virtual common-lisp:t)
 bool (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-msdf-pixel-range :class
  'text-server-extension :bind "_font_set_msdf_pixel_range" :hash 3411492887
  :virtual common-lisp:t)
 :void (font-rid rid) (msdf-pixel-range int))

(defgmethod
 (text-server-extension+%font-get-msdf-pixel-range :class
  'text-server-extension :bind "_font_get_msdf_pixel_range" :hash 2198884583
  :virtual common-lisp:t)
 int (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-msdf-size :class 'text-server-extension :bind
  "_font_set_msdf_size" :hash 3411492887 :virtual common-lisp:t)
 :void (font-rid rid) (msdf-size int))

(defgmethod
 (text-server-extension+%font-get-msdf-size :class 'text-server-extension :bind
  "_font_get_msdf_size" :hash 2198884583 :virtual common-lisp:t)
 int (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-fixed-size :class 'text-server-extension
  :bind "_font_set_fixed_size" :hash 3411492887 :virtual common-lisp:t)
 :void (font-rid rid) (fixed-size int))

(defgmethod
 (text-server-extension+%font-get-fixed-size :class 'text-server-extension
  :bind "_font_get_fixed_size" :hash 2198884583 :virtual common-lisp:t)
 int (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-fixed-size-scale-mode :class
  'text-server-extension :bind "_font_set_fixed_size_scale_mode" :hash
  1029390307 :virtual common-lisp:t)
 :void (font-rid rid) (fixed-size-scale-mode text-server+fixed-size-scale-mode))

(defgmethod
 (text-server-extension+%font-get-fixed-size-scale-mode :class
  'text-server-extension :bind "_font_get_fixed_size_scale_mode" :hash
  4113120379 :virtual common-lisp:t)
 text-server+fixed-size-scale-mode (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-allow-system-fallback :class
  'text-server-extension :bind "_font_set_allow_system_fallback" :hash
  1265174801 :virtual common-lisp:t)
 :void (font-rid rid) (allow-system-fallback bool))

(defgmethod
 (text-server-extension+%font-is-allow-system-fallback :class
  'text-server-extension :bind "_font_is_allow_system_fallback" :hash
  4155700596 :virtual common-lisp:t)
 bool (font-rid rid))

(defgmethod
 (text-server-extension+%font-clear-system-fallback-cache :class
  'text-server-extension :bind "_font_clear_system_fallback_cache" :hash
  3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (text-server-extension+%font-set-force-autohinter :class
  'text-server-extension :bind "_font_set_force_autohinter" :hash 1265174801
  :virtual common-lisp:t)
 :void (font-rid rid) (force-autohinter bool))

(defgmethod
 (text-server-extension+%font-is-force-autohinter :class 'text-server-extension
  :bind "_font_is_force_autohinter" :hash 4155700596 :virtual common-lisp:t)
 bool (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-modulate-color-glyphs :class
  'text-server-extension :bind "_font_set_modulate_color_glyphs" :hash
  1265174801 :virtual common-lisp:t)
 :void (font-rid rid) (modulate bool))

(defgmethod
 (text-server-extension+%font-is-modulate-color-glyphs :class
  'text-server-extension :bind "_font_is_modulate_color_glyphs" :hash
  4155700596 :virtual common-lisp:t)
 bool (font-rid rid))

(defgmethod
 (text-server-extension+%font-get-palette-count :class 'text-server-extension
  :bind "_font_get_palette_count" :hash 2198884583 :virtual common-lisp:t)
 int (font-rid rid))

(defgmethod
 (text-server-extension+%font-get-palette-name :class 'text-server-extension
  :bind "_font_get_palette_name" :hash 1464764419 :virtual common-lisp:t)
 string (font-rid rid) (index int))

(defgmethod
 (text-server-extension+%font-get-palette-colors :class 'text-server-extension
  :bind "_font_get_palette_colors" :hash 1595517857 :virtual common-lisp:t)
 packed-color-array (font-rid rid) (index int))

(defgmethod
 (text-server-extension+%font-set-palette-custom-colors :class
  'text-server-extension :bind "_font_set_palette_custom_colors" :hash
  4037098590 :virtual common-lisp:t)
 :void (font-rid rid) (colors packed-color-array))

(defgmethod
 (text-server-extension+%font-get-palette-custom-colors :class
  'text-server-extension :bind "_font_get_palette_custom_colors" :hash
  1569415609 :virtual common-lisp:t)
 packed-color-array (font-rid rid))

(defgmethod
 (text-server-extension+%font-get-used-palette :class 'text-server-extension
  :bind "_font_get_used_palette" :hash 2198884583 :virtual common-lisp:t)
 int (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-used-palette :class 'text-server-extension
  :bind "_font_set_used_palette" :hash 3411492887 :virtual common-lisp:t)
 :void (font-rid rid) (index int))

(defgmethod
 (text-server-extension+%font-set-hinting :class 'text-server-extension :bind
  "_font_set_hinting" :hash 1520010864 :virtual common-lisp:t)
 :void (font-rid rid) (hinting text-server+hinting))

(defgmethod
 (text-server-extension+%font-get-hinting :class 'text-server-extension :bind
  "_font_get_hinting" :hash 3971592737 :virtual common-lisp:t)
 text-server+hinting (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-subpixel-positioning :class
  'text-server-extension :bind "_font_set_subpixel_positioning" :hash
  3830459669 :virtual common-lisp:t)
 :void (font-rid rid) (subpixel-positioning text-server+subpixel-positioning))

(defgmethod
 (text-server-extension+%font-get-subpixel-positioning :class
  'text-server-extension :bind "_font_get_subpixel_positioning" :hash
  2752233671 :virtual common-lisp:t)
 text-server+subpixel-positioning (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-keep-rounding-remainders :class
  'text-server-extension :bind "_font_set_keep_rounding_remainders" :hash
  1265174801 :virtual common-lisp:t)
 :void (font-rid rid) (keep-rounding-remainders bool))

(defgmethod
 (text-server-extension+%font-get-keep-rounding-remainders :class
  'text-server-extension :bind "_font_get_keep_rounding_remainders" :hash
  4155700596 :virtual common-lisp:t)
 bool (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-embolden :class 'text-server-extension :bind
  "_font_set_embolden" :hash 1794382983 :virtual common-lisp:t)
 :void (font-rid rid) (strength float))

(defgmethod
 (text-server-extension+%font-get-embolden :class 'text-server-extension :bind
  "_font_get_embolden" :hash 866169185 :virtual common-lisp:t)
 float (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-spacing :class 'text-server-extension :bind
  "_font_set_spacing" :hash 1307259930 :virtual common-lisp:t)
 :void (font-rid rid) (spacing text-server+spacing-type) (value int))

(defgmethod
 (text-server-extension+%font-get-spacing :class 'text-server-extension :bind
  "_font_get_spacing" :hash 1213653558 :virtual common-lisp:t)
 int (font-rid rid) (spacing text-server+spacing-type))

(defgmethod
 (text-server-extension+%font-set-baseline-offset :class 'text-server-extension
  :bind "_font_set_baseline_offset" :hash 1794382983 :virtual common-lisp:t)
 :void (font-rid rid) (baseline-offset float))

(defgmethod
 (text-server-extension+%font-get-baseline-offset :class 'text-server-extension
  :bind "_font_get_baseline_offset" :hash 866169185 :virtual common-lisp:t)
 float (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-transform :class 'text-server-extension :bind
  "_font_set_transform" :hash 1246044741 :virtual common-lisp:t)
 :void (font-rid rid) (transform transform-2d))

(defgmethod
 (text-server-extension+%font-get-transform :class 'text-server-extension :bind
  "_font_get_transform" :hash 213527486 :virtual common-lisp:t)
 transform-2d (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-variation-coordinates :class
  'text-server-extension :bind "_font_set_variation_coordinates" :hash
  1217542888 :virtual common-lisp:t)
 :void (font-rid rid) (variation-coordinates dictionary))

(defgmethod
 (text-server-extension+%font-get-variation-coordinates :class
  'text-server-extension :bind "_font_get_variation_coordinates" :hash
  1882737106 :virtual common-lisp:t)
 dictionary (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-oversampling :class 'text-server-extension
  :bind "_font_set_oversampling" :hash 1794382983 :virtual common-lisp:t)
 :void (font-rid rid) (oversampling float))

(defgmethod
 (text-server-extension+%font-get-oversampling :class 'text-server-extension
  :bind "_font_get_oversampling" :hash 866169185 :virtual common-lisp:t)
 float (font-rid rid))

(defgmethod
 (text-server-extension+%font-get-size-cache-list :class 'text-server-extension
  :bind "_font_get_size_cache_list" :hash 2684255073 :virtual common-lisp:t)
 array (font-rid rid))

(defgmethod
 (text-server-extension+%font-clear-size-cache :class 'text-server-extension
  :bind "_font_clear_size_cache" :hash 2722037293 :virtual common-lisp:t)
 :void (font-rid rid))

(defgmethod
 (text-server-extension+%font-remove-size-cache :class 'text-server-extension
  :bind "_font_remove_size_cache" :hash 2450610377 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i))

(defgmethod
 (text-server-extension+%font-get-size-cache-info :class 'text-server-extension
  :bind "_font_get_size_cache_info" :hash 2684255073 :virtual common-lisp:t)
 array (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-ascent :class 'text-server-extension :bind
  "_font_set_ascent" :hash 1892459533 :virtual common-lisp:t)
 :void (font-rid rid) (size int) (ascent float))

(defgmethod
 (text-server-extension+%font-get-ascent :class 'text-server-extension :bind
  "_font_get_ascent" :hash 755457166 :virtual common-lisp:t)
 float (font-rid rid) (size int))

(defgmethod
 (text-server-extension+%font-set-descent :class 'text-server-extension :bind
  "_font_set_descent" :hash 1892459533 :virtual common-lisp:t)
 :void (font-rid rid) (size int) (descent float))

(defgmethod
 (text-server-extension+%font-get-descent :class 'text-server-extension :bind
  "_font_get_descent" :hash 755457166 :virtual common-lisp:t)
 float (font-rid rid) (size int))

(defgmethod
 (text-server-extension+%font-set-underline-position :class
  'text-server-extension :bind "_font_set_underline_position" :hash 1892459533
  :virtual common-lisp:t)
 :void (font-rid rid) (size int) (underline-position float))

(defgmethod
 (text-server-extension+%font-get-underline-position :class
  'text-server-extension :bind "_font_get_underline_position" :hash 755457166
  :virtual common-lisp:t)
 float (font-rid rid) (size int))

(defgmethod
 (text-server-extension+%font-set-underline-thickness :class
  'text-server-extension :bind "_font_set_underline_thickness" :hash 1892459533
  :virtual common-lisp:t)
 :void (font-rid rid) (size int) (underline-thickness float))

(defgmethod
 (text-server-extension+%font-get-underline-thickness :class
  'text-server-extension :bind "_font_get_underline_thickness" :hash 755457166
  :virtual common-lisp:t)
 float (font-rid rid) (size int))

(defgmethod
 (text-server-extension+%font-set-scale :class 'text-server-extension :bind
  "_font_set_scale" :hash 1892459533 :virtual common-lisp:t)
 :void (font-rid rid) (size int) (scale float))

(defgmethod
 (text-server-extension+%font-get-scale :class 'text-server-extension :bind
  "_font_get_scale" :hash 755457166 :virtual common-lisp:t)
 float (font-rid rid) (size int))

(defgmethod
 (text-server-extension+%font-get-texture-count :class 'text-server-extension
  :bind "_font_get_texture_count" :hash 1311001310 :virtual common-lisp:t)
 int (font-rid rid) (size vector-2i))

(defgmethod
 (text-server-extension+%font-clear-textures :class 'text-server-extension
  :bind "_font_clear_textures" :hash 2450610377 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i))

(defgmethod
 (text-server-extension+%font-remove-texture :class 'text-server-extension
  :bind "_font_remove_texture" :hash 3810512262 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i) (texture-index int))

(defgmethod
 (text-server-extension+%font-set-texture-image :class 'text-server-extension
  :bind "_font_set_texture_image" :hash 2354485091 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i) (texture-index int) (image image))

(defgmethod
 (text-server-extension+%font-get-texture-image :class 'text-server-extension
  :bind "_font_get_texture_image" :hash 2451761155 :virtual common-lisp:t)
 image (font-rid rid) (size vector-2i) (texture-index int))

(defgmethod
 (text-server-extension+%font-set-texture-offsets :class 'text-server-extension
  :bind "_font_set_texture_offsets" :hash 3005398047 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i) (texture-index int)
 (offset packed-int-32array))

(defgmethod
 (text-server-extension+%font-get-texture-offsets :class 'text-server-extension
  :bind "_font_get_texture_offsets" :hash 3420028887 :virtual common-lisp:t)
 packed-int-32array (font-rid rid) (size vector-2i) (texture-index int))

(defgmethod
 (text-server-extension+%font-get-glyph-list :class 'text-server-extension
  :bind "_font_get_glyph_list" :hash 46086620 :virtual common-lisp:t)
 packed-int-32array (font-rid rid) (size vector-2i))

(defgmethod
 (text-server-extension+%font-clear-glyphs :class 'text-server-extension :bind
  "_font_clear_glyphs" :hash 2450610377 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i))

(defgmethod
 (text-server-extension+%font-remove-glyph :class 'text-server-extension :bind
  "_font_remove_glyph" :hash 3810512262 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server-extension+%font-get-glyph-advance :class 'text-server-extension
  :bind "_font_get_glyph_advance" :hash 2555689501 :virtual common-lisp:t)
 vector-2 (font-rid rid) (size int) (glyph int))

(defgmethod
 (text-server-extension+%font-set-glyph-advance :class 'text-server-extension
  :bind "_font_set_glyph_advance" :hash 3219397315 :virtual common-lisp:t)
 :void (font-rid rid) (size int) (glyph int) (advance vector-2))

(defgmethod
 (text-server-extension+%font-get-glyph-offset :class 'text-server-extension
  :bind "_font_get_glyph_offset" :hash 513728628 :virtual common-lisp:t)
 vector-2 (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server-extension+%font-set-glyph-offset :class 'text-server-extension
  :bind "_font_set_glyph_offset" :hash 1812632090 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i) (glyph int) (offset vector-2))

(defgmethod
 (text-server-extension+%font-get-glyph-size :class 'text-server-extension
  :bind "_font_get_glyph_size" :hash 513728628 :virtual common-lisp:t)
 vector-2 (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server-extension+%font-set-glyph-size :class 'text-server-extension
  :bind "_font_set_glyph_size" :hash 1812632090 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i) (glyph int) (gl-size vector-2))

(defgmethod
 (text-server-extension+%font-get-glyph-uv-rect :class 'text-server-extension
  :bind "_font_get_glyph_uv_rect" :hash 2274268786 :virtual common-lisp:t)
 rect-2 (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server-extension+%font-set-glyph-uv-rect :class 'text-server-extension
  :bind "_font_set_glyph_uv_rect" :hash 1973324081 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i) (glyph int) (uv-rect rect-2))

(defgmethod
 (text-server-extension+%font-get-glyph-texture-idx :class
  'text-server-extension :bind "_font_get_glyph_texture_idx" :hash 4292800474
  :virtual common-lisp:t)
 int (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server-extension+%font-set-glyph-texture-idx :class
  'text-server-extension :bind "_font_set_glyph_texture_idx" :hash 4254580980
  :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i) (glyph int) (texture-idx int))

(defgmethod
 (text-server-extension+%font-get-glyph-texture-rid :class
  'text-server-extension :bind "_font_get_glyph_texture_rid" :hash 1451696141
  :virtual common-lisp:t)
 rid (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server-extension+%font-get-glyph-texture-size :class
  'text-server-extension :bind "_font_get_glyph_texture_size" :hash 513728628
  :virtual common-lisp:t)
 vector-2 (font-rid rid) (size vector-2i) (glyph int))

(defgmethod
 (text-server-extension+%font-get-glyph-contours :class 'text-server-extension
  :bind "_font_get_glyph_contours" :hash 2903964473 :virtual common-lisp:t)
 dictionary (font-rid rid) (size int) (index int))

(defgmethod
 (text-server-extension+%font-get-kerning-list :class 'text-server-extension
  :bind "_font_get_kerning_list" :hash 1778388067 :virtual common-lisp:t)
 array (font-rid rid) (size int))

(defgmethod
 (text-server-extension+%font-clear-kerning-map :class 'text-server-extension
  :bind "_font_clear_kerning_map" :hash 3411492887 :virtual common-lisp:t)
 :void (font-rid rid) (size int))

(defgmethod
 (text-server-extension+%font-remove-kerning :class 'text-server-extension
  :bind "_font_remove_kerning" :hash 2141860016 :virtual common-lisp:t)
 :void (font-rid rid) (size int) (glyph-pair vector-2i))

(defgmethod
 (text-server-extension+%font-set-kerning :class 'text-server-extension :bind
  "_font_set_kerning" :hash 3630965883 :virtual common-lisp:t)
 :void (font-rid rid) (size int) (glyph-pair vector-2i) (kerning vector-2))

(defgmethod
 (text-server-extension+%font-get-kerning :class 'text-server-extension :bind
  "_font_get_kerning" :hash 1019980169 :virtual common-lisp:t)
 vector-2 (font-rid rid) (size int) (glyph-pair vector-2i))

(defgmethod
 (text-server-extension+%font-get-glyph-index :class 'text-server-extension
  :bind "_font_get_glyph_index" :hash 1765635060 :virtual common-lisp:t)
 int (font-rid rid) (size int) (char int) (variation-selector int))

(defgmethod
 (text-server-extension+%font-get-char-from-glyph-index :class
  'text-server-extension :bind "_font_get_char_from_glyph_index" :hash
  2156738276 :virtual common-lisp:t)
 int (font-rid rid) (size int) (glyph-index int))

(defgmethod
 (text-server-extension+%font-has-char :class 'text-server-extension :bind
  "_font_has_char" :hash 3120086654 :virtual common-lisp:t)
 bool (font-rid rid) (char int))

(defgmethod
 (text-server-extension+%font-get-supported-chars :class 'text-server-extension
  :bind "_font_get_supported_chars" :hash 642473191 :virtual common-lisp:t)
 string (font-rid rid))

(defgmethod
 (text-server-extension+%font-get-supported-glyphs :class
  'text-server-extension :bind "_font_get_supported_glyphs" :hash 788230395
  :virtual common-lisp:t)
 packed-int-32array (font-rid rid))

(defgmethod
 (text-server-extension+%font-render-range :class 'text-server-extension :bind
  "_font_render_range" :hash 4254580980 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i) (start int) (end int))

(defgmethod
 (text-server-extension+%font-render-glyph :class 'text-server-extension :bind
  "_font_render_glyph" :hash 3810512262 :virtual common-lisp:t)
 :void (font-rid rid) (size vector-2i) (index int))

(defgmethod
 (text-server-extension+%font-draw-glyph :class 'text-server-extension :bind
  "_font_draw_glyph" :hash 404525066 :virtual common-lisp:t)
 :void (font-rid rid) (canvas rid) (size int) (pos vector-2) (index int)
 (color color) (oversampling float))

(defgmethod
 (text-server-extension+%font-draw-glyph-outline :class 'text-server-extension
  :bind "_font_draw_glyph_outline" :hash 940535541 :virtual common-lisp:t)
 :void (font-rid rid) (canvas rid) (size int) (outline-size int) (pos vector-2)
 (index int) (color color) (oversampling float))

(defgmethod
 (text-server-extension+%font-is-language-supported :class
  'text-server-extension :bind "_font_is_language_supported" :hash 3199320846
  :virtual common-lisp:t)
 bool (font-rid rid) (language string))

(defgmethod
 (text-server-extension+%font-set-language-support-override :class
  'text-server-extension :bind "_font_set_language_support_override" :hash
  2313957094 :virtual common-lisp:t)
 :void (font-rid rid) (language string) (supported bool))

(defgmethod
 (text-server-extension+%font-get-language-support-override :class
  'text-server-extension :bind "_font_get_language_support_override" :hash
  2829184646 :virtual common-lisp:t)
 bool (font-rid rid) (language string))

(defgmethod
 (text-server-extension+%font-remove-language-support-override :class
  'text-server-extension :bind "_font_remove_language_support_override" :hash
  2726140452 :virtual common-lisp:t)
 :void (font-rid rid) (language string))

(defgmethod
 (text-server-extension+%font-get-language-support-overrides :class
  'text-server-extension :bind "_font_get_language_support_overrides" :hash
  2801473409 :virtual common-lisp:t)
 packed-string-array (font-rid rid))

(defgmethod
 (text-server-extension+%font-is-script-supported :class 'text-server-extension
  :bind "_font_is_script_supported" :hash 3199320846 :virtual common-lisp:t)
 bool (font-rid rid) (script string))

(defgmethod
 (text-server-extension+%font-set-script-support-override :class
  'text-server-extension :bind "_font_set_script_support_override" :hash
  2313957094 :virtual common-lisp:t)
 :void (font-rid rid) (script string) (supported bool))

(defgmethod
 (text-server-extension+%font-get-script-support-override :class
  'text-server-extension :bind "_font_get_script_support_override" :hash
  2829184646 :virtual common-lisp:t)
 bool (font-rid rid) (script string))

(defgmethod
 (text-server-extension+%font-remove-script-support-override :class
  'text-server-extension :bind "_font_remove_script_support_override" :hash
  2726140452 :virtual common-lisp:t)
 :void (font-rid rid) (script string))

(defgmethod
 (text-server-extension+%font-get-script-support-overrides :class
  'text-server-extension :bind "_font_get_script_support_overrides" :hash
  2801473409 :virtual common-lisp:t)
 packed-string-array (font-rid rid))

(defgmethod
 (text-server-extension+%font-set-opentype-feature-overrides :class
  'text-server-extension :bind "_font_set_opentype_feature_overrides" :hash
  1217542888 :virtual common-lisp:t)
 :void (font-rid rid) (overrides dictionary))

(defgmethod
 (text-server-extension+%font-get-opentype-feature-overrides :class
  'text-server-extension :bind "_font_get_opentype_feature_overrides" :hash
  1882737106 :virtual common-lisp:t)
 dictionary (font-rid rid))

(defgmethod
 (text-server-extension+%font-supported-feature-list :class
  'text-server-extension :bind "_font_supported_feature_list" :hash 1882737106
  :virtual common-lisp:t)
 dictionary (font-rid rid))

(defgmethod
 (text-server-extension+%font-supported-variation-list :class
  'text-server-extension :bind "_font_supported_variation_list" :hash
  1882737106 :virtual common-lisp:t)
 dictionary (font-rid rid))

(defgmethod
 (text-server-extension+%font-get-global-oversampling :class
  'text-server-extension :bind "_font_get_global_oversampling" :hash 1740695150
  :virtual common-lisp:t)
 float)

(defgmethod
 (text-server-extension+%font-set-global-oversampling :class
  'text-server-extension :bind "_font_set_global_oversampling" :hash 373806689
  :virtual common-lisp:t)
 :void (oversampling float))

(defgmethod
 (text-server-extension+%reference-oversampling-level :class
  'text-server-extension :bind "_reference_oversampling_level" :hash 373806689
  :virtual common-lisp:t)
 :void (oversampling float))

(defgmethod
 (text-server-extension+%unreference-oversampling-level :class
  'text-server-extension :bind "_unreference_oversampling_level" :hash
  373806689 :virtual common-lisp:t)
 :void (oversampling float))

(defgmethod
 (text-server-extension+%get-hex-code-box-size :class 'text-server-extension
  :bind "_get_hex_code_box_size" :hash 3016396712 :virtual common-lisp:t)
 vector-2 (size int) (index int))

(defgmethod
 (text-server-extension+%draw-hex-code-box :class 'text-server-extension :bind
  "_draw_hex_code_box" :hash 1602046441 :virtual common-lisp:t)
 :void (canvas rid) (size int) (pos vector-2) (index int) (color color))

(defgmethod
 (text-server-extension+%create-shaped-text :class 'text-server-extension :bind
  "_create_shaped_text" :hash 1431128392 :virtual common-lisp:t)
 rid (direction text-server+direction) (orientation text-server+orientation))

(defgmethod
 (text-server-extension+%shaped-text-clear :class 'text-server-extension :bind
  "_shaped_text_clear" :hash 2722037293 :virtual common-lisp:t)
 :void (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-duplicate :class 'text-server-extension
  :bind "_shaped_text_duplicate" :hash 41030802 :virtual common-lisp:t)
 rid (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-set-direction :class
  'text-server-extension :bind "_shaped_text_set_direction" :hash 4276135416
  :virtual common-lisp:t)
 :void (shaped rid) (direction text-server+direction))

(defgmethod
 (text-server-extension+%shaped-text-get-direction :class
  'text-server-extension :bind "_shaped_text_get_direction" :hash 3065904362
  :virtual common-lisp:t)
 text-server+direction (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-inferred-direction :class
  'text-server-extension :bind "_shaped_text_get_inferred_direction" :hash
  3065904362 :virtual common-lisp:t)
 text-server+direction (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-set-bidi-override :class
  'text-server-extension :bind "_shaped_text_set_bidi_override" :hash 684822712
  :virtual common-lisp:t)
 :void (shaped rid) (override array))

(defgmethod
 (text-server-extension+%shaped-text-set-custom-punctuation :class
  'text-server-extension :bind "_shaped_text_set_custom_punctuation" :hash
  2726140452 :virtual common-lisp:t)
 :void (shaped rid) (punct string))

(defgmethod
 (text-server-extension+%shaped-text-get-custom-punctuation :class
  'text-server-extension :bind "_shaped_text_get_custom_punctuation" :hash
  642473191 :virtual common-lisp:t)
 string (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-set-custom-ellipsis :class
  'text-server-extension :bind "_shaped_text_set_custom_ellipsis" :hash
  3411492887 :virtual common-lisp:t)
 :void (shaped rid) (char int))

(defgmethod
 (text-server-extension+%shaped-text-get-custom-ellipsis :class
  'text-server-extension :bind "_shaped_text_get_custom_ellipsis" :hash
  2198884583 :virtual common-lisp:t)
 int (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-set-orientation :class
  'text-server-extension :bind "_shaped_text_set_orientation" :hash 2306444742
  :virtual common-lisp:t)
 :void (shaped rid) (orientation text-server+orientation))

(defgmethod
 (text-server-extension+%shaped-text-get-orientation :class
  'text-server-extension :bind "_shaped_text_get_orientation" :hash 3142708106
  :virtual common-lisp:t)
 text-server+orientation (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-set-preserve-invalid :class
  'text-server-extension :bind "_shaped_text_set_preserve_invalid" :hash
  1265174801 :virtual common-lisp:t)
 :void (shaped rid) (enabled bool))

(defgmethod
 (text-server-extension+%shaped-text-get-preserve-invalid :class
  'text-server-extension :bind "_shaped_text_get_preserve_invalid" :hash
  4155700596 :virtual common-lisp:t)
 bool (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-set-preserve-control :class
  'text-server-extension :bind "_shaped_text_set_preserve_control" :hash
  1265174801 :virtual common-lisp:t)
 :void (shaped rid) (enabled bool))

(defgmethod
 (text-server-extension+%shaped-text-get-preserve-control :class
  'text-server-extension :bind "_shaped_text_get_preserve_control" :hash
  4155700596 :virtual common-lisp:t)
 bool (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-set-spacing :class 'text-server-extension
  :bind "_shaped_text_set_spacing" :hash 1307259930 :virtual common-lisp:t)
 :void (shaped rid) (spacing text-server+spacing-type) (value int))

(defgmethod
 (text-server-extension+%shaped-text-get-spacing :class 'text-server-extension
  :bind "_shaped_text_get_spacing" :hash 1213653558 :virtual common-lisp:t)
 int (shaped rid) (spacing text-server+spacing-type))

(defgmethod
 (text-server-extension+%shaped-text-add-string :class 'text-server-extension
  :bind "_shaped_text_add_string" :hash 875249313 :virtual common-lisp:t)
 bool (shaped rid) (text string) (fonts array) (size int)
 (opentype-features dictionary) (language string) (meta variant))

(defgmethod
 (text-server-extension+%shaped-text-add-object :class 'text-server-extension
  :bind "_shaped_text_add_object" :hash 2452224230 :virtual common-lisp:t)
 bool (shaped rid) (key variant) (size vector-2)
 (inline-align inline-alignment) (length int) (baseline float))

(defgmethod
 (text-server-extension+%shaped-text-resize-object :class
  'text-server-extension :bind "_shaped_text_resize_object" :hash 2747466775
  :virtual common-lisp:t)
 bool (shaped rid) (key variant) (size vector-2)
 (inline-align inline-alignment) (baseline float))

(defgmethod
 (text-server-extension+%shaped-text-has-object :class 'text-server-extension
  :bind "_shaped_text_has_object" :hash 2360964694 :virtual common-lisp:t)
 bool (shaped rid) (key variant))

(defgmethod
 (text-server-extension+%shaped-get-text :class 'text-server-extension :bind
  "_shaped_get_text" :hash 642473191 :virtual common-lisp:t)
 string (shaped rid))

(defgmethod
 (text-server-extension+%shaped-get-span-count :class 'text-server-extension
  :bind "_shaped_get_span_count" :hash 2198884583 :virtual common-lisp:t)
 int (shaped rid))

(defgmethod
 (text-server-extension+%shaped-get-span-meta :class 'text-server-extension
  :bind "_shaped_get_span_meta" :hash 4069510997 :virtual common-lisp:t)
 variant (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-get-span-embedded-object :class
  'text-server-extension :bind "_shaped_get_span_embedded_object" :hash
  4069510997 :virtual common-lisp:t)
 variant (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-get-span-text :class 'text-server-extension
  :bind "_shaped_get_span_text" :hash 1464764419 :virtual common-lisp:t)
 string (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-get-span-object :class 'text-server-extension
  :bind "_shaped_get_span_object" :hash 4069510997 :virtual common-lisp:t)
 variant (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-set-span-update-font :class
  'text-server-extension :bind "_shaped_set_span_update_font" :hash 2569459151
  :virtual common-lisp:t)
 :void (shaped rid) (index int) (fonts array) (size int)
 (opentype-features dictionary))

(defgmethod
 (text-server-extension+%shaped-get-run-count :class 'text-server-extension
  :bind "_shaped_get_run_count" :hash 2198884583 :virtual common-lisp:t)
 int (shaped rid))

(defgmethod
 (text-server-extension+%shaped-get-run-text :class 'text-server-extension
  :bind "_shaped_get_run_text" :hash 1464764419 :virtual common-lisp:t)
 string (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-get-run-range :class 'text-server-extension
  :bind "_shaped_get_run_range" :hash 4069534484 :virtual common-lisp:t)
 vector-2i (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-get-run-glyph-range :class
  'text-server-extension :bind "_shaped_get_run_glyph_range" :hash 4069534484
  :virtual common-lisp:t)
 vector-2i (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-get-run-font-rid :class 'text-server-extension
  :bind "_shaped_get_run_font_rid" :hash 1066463050 :virtual common-lisp:t)
 rid (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-get-run-font-size :class 'text-server-extension
  :bind "_shaped_get_run_font_size" :hash 1120910005 :virtual common-lisp:t)
 int (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-get-run-language :class 'text-server-extension
  :bind "_shaped_get_run_language" :hash 1464764419 :virtual common-lisp:t)
 string (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-get-run-direction :class 'text-server-extension
  :bind "_shaped_get_run_direction" :hash 2413896864 :virtual common-lisp:t)
 text-server+direction (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-get-run-object :class 'text-server-extension
  :bind "_shaped_get_run_object" :hash 4069510997 :virtual common-lisp:t)
 variant (shaped rid) (index int))

(defgmethod
 (text-server-extension+%shaped-text-substr :class 'text-server-extension :bind
  "_shaped_text_substr" :hash 1937682086 :virtual common-lisp:t)
 rid (shaped rid) (start int) (length int))

(defgmethod
 (text-server-extension+%shaped-text-get-parent :class 'text-server-extension
  :bind "_shaped_text_get_parent" :hash 3814569979 :virtual common-lisp:t)
 rid (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-fit-to-width :class 'text-server-extension
  :bind "_shaped_text_fit_to_width" :hash 1426448222 :virtual common-lisp:t)
 float (shaped rid) (width float)
 (justification-flags text-server+justification-flag))

(defgmethod
 (text-server-extension+%shaped-text-tab-align :class 'text-server-extension
  :bind "_shaped_text_tab_align" :hash 1283669550 :virtual common-lisp:t)
 float (shaped rid) (tab-stops packed-float-32array))

(defgmethod
 (text-server-extension+%shaped-text-shape :class 'text-server-extension :bind
  "_shaped_text_shape" :hash 3521089500 :virtual common-lisp:t)
 bool (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-update-breaks :class
  'text-server-extension :bind "_shaped_text_update_breaks" :hash 3521089500
  :virtual common-lisp:t)
 bool (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-update-justification-ops :class
  'text-server-extension :bind "_shaped_text_update_justification_ops" :hash
  3521089500 :virtual common-lisp:t)
 bool (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-is-ready :class 'text-server-extension
  :bind "_shaped_text_is_ready" :hash 4155700596 :virtual common-lisp:t)
 bool (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-glyphs :class 'text-server-extension
  :bind "_shaped_text_get_glyphs" :hash 2198884583 :virtual common-lisp:t)
 (:pointer glyph) (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-sort-logical :class 'text-server-extension
  :bind "_shaped_text_sort_logical" :hash 3917799429 :virtual common-lisp:t)
 (:pointer glyph) (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-glyph-count :class
  'text-server-extension :bind "_shaped_text_get_glyph_count" :hash 2198884583
  :virtual common-lisp:t)
 int (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-range :class 'text-server-extension
  :bind "_shaped_text_get_range" :hash 733700038 :virtual common-lisp:t)
 vector-2i (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-line-breaks-adv :class
  'text-server-extension :bind "_shaped_text_get_line_breaks_adv" :hash
  1488467363 :virtual common-lisp:t)
 packed-int-32array (shaped rid) (width packed-float-32array) (start int)
 (once bool) (break-flags text-server+line-break-flag))

(defgmethod
 (text-server-extension+%shaped-text-get-line-breaks :class
  'text-server-extension :bind "_shaped_text_get_line_breaks" :hash 3131311977
  :virtual common-lisp:t)
 packed-int-32array (shaped rid) (width float) (start int)
 (break-flags text-server+line-break-flag))

(defgmethod
 (text-server-extension+%shaped-text-get-word-breaks :class
  'text-server-extension :bind "_shaped_text_get_word_breaks" :hash 2423529412
  :virtual common-lisp:t)
 packed-int-32array (shaped rid) (grapheme-flags text-server+grapheme-flag)
 (skip-grapheme-flags text-server+grapheme-flag))

(defgmethod
 (text-server-extension+%shaped-text-get-trim-pos :class 'text-server-extension
  :bind "_shaped_text_get_trim_pos" :hash 2198884583 :virtual common-lisp:t)
 int (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-ellipsis-pos :class
  'text-server-extension :bind "_shaped_text_get_ellipsis_pos" :hash 2198884583
  :virtual common-lisp:t)
 int (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-ellipsis-glyph-count :class
  'text-server-extension :bind "_shaped_text_get_ellipsis_glyph_count" :hash
  2198884583 :virtual common-lisp:t)
 int (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-ellipsis-glyphs :class
  'text-server-extension :bind "_shaped_text_get_ellipsis_glyphs" :hash
  2198884583 :virtual common-lisp:t)
 (:pointer glyph) (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-overrun-trim-to-width :class
  'text-server-extension :bind "_shaped_text_overrun_trim_to_width" :hash
  3364950921 :virtual common-lisp:t)
 :void (shaped rid) (width float) (trim-flags text-server+text-overrun-flag))

(defgmethod
 (text-server-extension+%shaped-text-get-objects :class 'text-server-extension
  :bind "_shaped_text_get_objects" :hash 2684255073 :virtual common-lisp:t)
 array (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-object-rect :class
  'text-server-extension :bind "_shaped_text_get_object_rect" :hash 447978354
  :virtual common-lisp:t)
 rect-2 (shaped rid) (key variant))

(defgmethod
 (text-server-extension+%shaped-text-get-object-range :class
  'text-server-extension :bind "_shaped_text_get_object_range" :hash 2524675647
  :virtual common-lisp:t)
 vector-2i (shaped rid) (key variant))

(defgmethod
 (text-server-extension+%shaped-text-get-object-glyph :class
  'text-server-extension :bind "_shaped_text_get_object_glyph" :hash 1260085030
  :virtual common-lisp:t)
 int (shaped rid) (key variant))

(defgmethod
 (text-server-extension+%shaped-text-get-size :class 'text-server-extension
  :bind "_shaped_text_get_size" :hash 2440833711 :virtual common-lisp:t)
 vector-2 (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-ascent :class 'text-server-extension
  :bind "_shaped_text_get_ascent" :hash 866169185 :virtual common-lisp:t)
 float (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-descent :class 'text-server-extension
  :bind "_shaped_text_get_descent" :hash 866169185 :virtual common-lisp:t)
 float (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-width :class 'text-server-extension
  :bind "_shaped_text_get_width" :hash 866169185 :virtual common-lisp:t)
 float (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-underline-position :class
  'text-server-extension :bind "_shaped_text_get_underline_position" :hash
  866169185 :virtual common-lisp:t)
 float (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-underline-thickness :class
  'text-server-extension :bind "_shaped_text_get_underline_thickness" :hash
  866169185 :virtual common-lisp:t)
 float (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-get-dominant-direction-in-range :class
  'text-server-extension :bind "_shaped_text_get_dominant_direction_in_range"
  :hash 2156738276 :virtual common-lisp:t)
 int (shaped rid) (start int) (end int))

(defgmethod
 (text-server-extension+%shaped-text-get-carets :class 'text-server-extension
  :bind "_shaped_text_get_carets" :hash 1191777527 :virtual common-lisp:t)
 :void (shaped rid) (position int) (r-caret (:pointer caret-info)))

(defgmethod
 (text-server-extension+%shaped-text-get-selection :class
  'text-server-extension :bind "_shaped_text_get_selection" :hash 3714187733
  :virtual common-lisp:t)
 packed-vector-2array (shaped rid) (start int) (end int))

(defgmethod
 (text-server-extension+%shaped-text-hit-test-grapheme :class
  'text-server-extension :bind "_shaped_text_hit_test_grapheme" :hash
  3149310417 :virtual common-lisp:t)
 int (shaped rid) (coord float))

(defgmethod
 (text-server-extension+%shaped-text-hit-test-position :class
  'text-server-extension :bind "_shaped_text_hit_test_position" :hash
  3149310417 :virtual common-lisp:t)
 int (shaped rid) (coord float))

(defgmethod
 (text-server-extension+%shaped-text-draw :class 'text-server-extension :bind
  "_shaped_text_draw" :hash 2079930245 :virtual common-lisp:t)
 :void (shaped rid) (canvas rid) (pos vector-2) (clip-l float) (clip-r float)
 (color color) (oversampling float))

(defgmethod
 (text-server-extension+%shaped-text-draw-outline :class 'text-server-extension
  :bind "_shaped_text_draw_outline" :hash 601976754 :virtual common-lisp:t)
 :void (shaped rid) (canvas rid) (pos vector-2) (clip-l float) (clip-r float)
 (outline-size int) (color color) (oversampling float))

(defgmethod
 (text-server-extension+%shaped-text-get-grapheme-bounds :class
  'text-server-extension :bind "_shaped_text_get_grapheme_bounds" :hash
  2546185844 :virtual common-lisp:t)
 vector-2 (shaped rid) (pos int))

(defgmethod
 (text-server-extension+%shaped-text-next-grapheme-pos :class
  'text-server-extension :bind "_shaped_text_next_grapheme_pos" :hash
  1120910005 :virtual common-lisp:t)
 int (shaped rid) (pos int))

(defgmethod
 (text-server-extension+%shaped-text-prev-grapheme-pos :class
  'text-server-extension :bind "_shaped_text_prev_grapheme_pos" :hash
  1120910005 :virtual common-lisp:t)
 int (shaped rid) (pos int))

(defgmethod
 (text-server-extension+%shaped-text-get-character-breaks :class
  'text-server-extension :bind "_shaped_text_get_character_breaks" :hash
  788230395 :virtual common-lisp:t)
 packed-int-32array (shaped rid))

(defgmethod
 (text-server-extension+%shaped-text-next-character-pos :class
  'text-server-extension :bind "_shaped_text_next_character_pos" :hash
  1120910005 :virtual common-lisp:t)
 int (shaped rid) (pos int))

(defgmethod
 (text-server-extension+%shaped-text-prev-character-pos :class
  'text-server-extension :bind "_shaped_text_prev_character_pos" :hash
  1120910005 :virtual common-lisp:t)
 int (shaped rid) (pos int))

(defgmethod
 (text-server-extension+%shaped-text-closest-character-pos :class
  'text-server-extension :bind "_shaped_text_closest_character_pos" :hash
  1120910005 :virtual common-lisp:t)
 int (shaped rid) (pos int))

(defgmethod
 (text-server-extension+%format-number :class 'text-server-extension :bind
  "_format_number" :hash 315676799 :virtual common-lisp:t)
 string (number string) (language string))

(defgmethod
 (text-server-extension+%parse-number :class 'text-server-extension :bind
  "_parse_number" :hash 315676799 :virtual common-lisp:t)
 string (number string) (language string))

(defgmethod
 (text-server-extension+%percent-sign :class 'text-server-extension :bind
  "_percent_sign" :hash 3135753539 :virtual common-lisp:t)
 string (language string))

(defgmethod
 (text-server-extension+%strip-diacritics :class 'text-server-extension :bind
  "_strip_diacritics" :hash 3135753539 :virtual common-lisp:t)
 string (string string))

(defgmethod
 (text-server-extension+%is-valid-identifier :class 'text-server-extension
  :bind "_is_valid_identifier" :hash 3927539163 :virtual common-lisp:t)
 bool (string string))

(defgmethod
 (text-server-extension+%is-valid-letter :class 'text-server-extension :bind
  "_is_valid_letter" :hash 1116898809 :virtual common-lisp:t)
 bool (unicode int))

(defgmethod
 (text-server-extension+%string-get-word-breaks :class 'text-server-extension
  :bind "_string_get_word_breaks" :hash 3658450588 :virtual common-lisp:t)
 packed-int-32array (string string) (language string) (chars-per-line int))

(defgmethod
 (text-server-extension+%string-get-character-breaks :class
  'text-server-extension :bind "_string_get_character_breaks" :hash 2509056759
  :virtual common-lisp:t)
 packed-int-32array (string string) (language string))

(defgmethod
 (text-server-extension+%is-confusable :class 'text-server-extension :bind
  "_is_confusable" :hash 1433197768 :virtual common-lisp:t)
 int (string string) (dict packed-string-array))

(defgmethod
 (text-server-extension+%spoof-check :class 'text-server-extension :bind
  "_spoof_check" :hash 3927539163 :virtual common-lisp:t)
 bool (string string))

(defgmethod
 (text-server-extension+%string-to-upper :class 'text-server-extension :bind
  "_string_to_upper" :hash 315676799 :virtual common-lisp:t)
 string (string string) (language string))

(defgmethod
 (text-server-extension+%string-to-lower :class 'text-server-extension :bind
  "_string_to_lower" :hash 315676799 :virtual common-lisp:t)
 string (string string) (language string))

(defgmethod
 (text-server-extension+%string-to-title :class 'text-server-extension :bind
  "_string_to_title" :hash 315676799 :virtual common-lisp:t)
 string (string string) (language string))

(defgmethod
 (text-server-extension+%parse-structured-text :class 'text-server-extension
  :bind "_parse_structured_text" :hash 3310685015 :virtual common-lisp:t)
 array (parser-type text-server+structured-text-parser) (args array)
 (text string))

(defgmethod
 (text-server-extension+%cleanup :class 'text-server-extension :bind "_cleanup"
  :hash 3218959716 :virtual common-lisp:t)
 :void)