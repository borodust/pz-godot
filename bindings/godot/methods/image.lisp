(common-lisp:in-package :%godot)


(defgmethod (image+get-width :class 'image :bind "get_width" :hash 3905245786)
 int)

(defgmethod
 (image+get-height :class 'image :bind "get_height" :hash 3905245786) int)

(defgmethod (image+get-size :class 'image :bind "get_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (image+has-mipmaps :class 'image :bind "has_mipmaps" :hash 36873697) bool)

(defgmethod
 (image+get-format :class 'image :bind "get_format" :hash 3847873762)
 image+format)

(defgmethod (image+get-data :class 'image :bind "get_data" :hash 2362200018)
 packed-byte-array)

(defgmethod
 (image+get-data-size :class 'image :bind "get_data_size" :hash 3905245786) int)

(defgmethod (image+convert :class 'image :bind "convert" :hash 2120693146)
 :void (format image+format))

(defgmethod
 (image+get-mipmap-count :class 'image :bind "get_mipmap_count" :hash
  3905245786)
 int)

(defgmethod
 (image+get-mipmap-offset :class 'image :bind "get_mipmap_offset" :hash
  923996154)
 int (mipmap int))

(defgmethod
 (image+resize-to-po2 :class 'image :bind "resize_to_po2" :hash 4189212329)
 :void (square bool) (interpolation image+interpolation))

(defgmethod (image+resize :class 'image :bind "resize" :hash 994498151) :void
 (width int) (height int) (interpolation image+interpolation))

(defgmethod (image+shrink-x2 :class 'image :bind "shrink_x2" :hash 3218959716)
 :void)

(defgmethod (image+crop :class 'image :bind "crop" :hash 3937882851) :void
 (width int) (height int))

(defgmethod (image+flip-x :class 'image :bind "flip_x" :hash 3218959716) :void)

(defgmethod (image+flip-y :class 'image :bind "flip_y" :hash 3218959716) :void)

(defgmethod
 (image+generate-mipmaps :class 'image :bind "generate_mipmaps" :hash
  1633102583)
 error (renormalize bool))

(defgmethod
 (image+clear-mipmaps :class 'image :bind "clear_mipmaps" :hash 3218959716)
 :void)

(defgmethod
 (image+create :class 'image :bind "create" :hash 986942177 :static
  common-lisp:t)
 image (width int) (height int) (use-mipmaps bool) (format image+format))

(defgmethod
 (image+create-empty :class 'image :bind "create_empty" :hash 986942177 :static
  common-lisp:t)
 image (width int) (height int) (use-mipmaps bool) (format image+format))

(defgmethod
 (image+create-from-data :class 'image :bind "create_from_data" :hash 299398494
  :static common-lisp:t)
 image (width int) (height int) (use-mipmaps bool) (format image+format)
 (data packed-byte-array))

(defgmethod (image+set-data :class 'image :bind "set_data" :hash 2740482212)
 :void (width int) (height int) (use-mipmaps bool) (format image+format)
 (data packed-byte-array))

(defgmethod (image+is-empty :class 'image :bind "is_empty" :hash 36873697) bool)

(defgmethod (image+load :class 'image :bind "load" :hash 166001499) error
 (path string))

(defgmethod
 (image+load-from-file :class 'image :bind "load_from_file" :hash 736337515
  :static common-lisp:t)
 image (path string))

(defgmethod (image+save-png :class 'image :bind "save_png" :hash 2113323047)
 error (path string))

(defgmethod
 (image+save-png-to-buffer :class 'image :bind "save_png_to_buffer" :hash
  2362200018)
 packed-byte-array)

(defgmethod (image+save-jpg :class 'image :bind "save_jpg" :hash 2800019068)
 error (path string) (quality float))

(defgmethod
 (image+save-jpg-to-buffer :class 'image :bind "save_jpg_to_buffer" :hash
  592235273)
 packed-byte-array (quality float))

(defgmethod (image+save-exr :class 'image :bind "save_exr" :hash 2018602448)
 error (path string) (grayscale bool) (color-image bool)
 (max-linear-value float))

(defgmethod
 (image+save-exr-to-buffer :class 'image :bind "save_exr_to_buffer" :hash
  1477518536)
 packed-byte-array (grayscale bool) (color-image bool) (max-linear-value float))

(defgmethod (image+save-dds :class 'image :bind "save_dds" :hash 2113323047)
 error (path string))

(defgmethod
 (image+save-dds-to-buffer :class 'image :bind "save_dds_to_buffer" :hash
  2362200018)
 packed-byte-array)

(defgmethod (image+save-webp :class 'image :bind "save_webp" :hash 2781156876)
 error (path string) (lossy bool) (quality float))

(defgmethod
 (image+save-webp-to-buffer :class 'image :bind "save_webp_to_buffer" :hash
  1214628238)
 packed-byte-array (lossy bool) (quality float))

(defgmethod
 (image+detect-alpha :class 'image :bind "detect_alpha" :hash 2030116505)
 image+alpha-mode)

(defgmethod
 (image+is-invisible :class 'image :bind "is_invisible" :hash 36873697) bool)

(defgmethod
 (image+detect-used-channels :class 'image :bind "detect_used_channels" :hash
  2703139984)
 image+used-channels (source image+compress-source))

(defgmethod (image+compress :class 'image :bind "compress" :hash 2975424957)
 error (mode image+compress-mode) (source image+compress-source)
 (astc-format image+astcformat))

(defgmethod
 (image+compress-from-channels :class 'image :bind "compress_from_channels"
  :hash 4212890953)
 error (mode image+compress-mode) (channels image+used-channels)
 (astc-format image+astcformat))

(defgmethod (image+decompress :class 'image :bind "decompress" :hash 166280745)
 error)

(defgmethod
 (image+is-compressed :class 'image :bind "is_compressed" :hash 36873697) bool)

(defgmethod (image+rotate-90 :class 'image :bind "rotate_90" :hash 1901204267)
 :void (direction clock-direction))

(defgmethod
 (image+rotate-180 :class 'image :bind "rotate_180" :hash 3218959716) :void)

(defgmethod
 (image+fix-alpha-edges :class 'image :bind "fix_alpha_edges" :hash 3218959716)
 :void)

(defgmethod
 (image+premultiply-alpha :class 'image :bind "premultiply_alpha" :hash
  3218959716)
 :void)

(defgmethod
 (image+srgb-to-linear :class 'image :bind "srgb_to_linear" :hash 3218959716)
 :void)

(defgmethod
 (image+linear-to-srgb :class 'image :bind "linear_to_srgb" :hash 3218959716)
 :void)

(defgmethod
 (image+normal-map-to-xy :class 'image :bind "normal_map_to_xy" :hash
  3218959716)
 :void)

(defgmethod
 (image+rgbe-to-srgb :class 'image :bind "rgbe_to_srgb" :hash 564927088) image)

(defgmethod
 (image+bump-map-to-normal-map :class 'image :bind "bump_map_to_normal_map"
  :hash 3423495036)
 :void (bump-scale float))

(defgmethod
 (image+compute-image-metrics :class 'image :bind "compute_image_metrics" :hash
  3080961247)
 dictionary (compared-image image) (use-luma bool))

(defgmethod (image+blit-rect :class 'image :bind "blit_rect" :hash 2903928755)
 :void (src image) (src-rect rect-2i) (dst vector-2i))

(defgmethod
 (image+blit-rect-mask :class 'image :bind "blit_rect_mask" :hash 3383581145)
 :void (src image) (mask image) (src-rect rect-2i) (dst vector-2i))

(defgmethod
 (image+blend-rect :class 'image :bind "blend_rect" :hash 2903928755) :void
 (src image) (src-rect rect-2i) (dst vector-2i))

(defgmethod
 (image+blend-rect-mask :class 'image :bind "blend_rect_mask" :hash 3383581145)
 :void (src image) (mask image) (src-rect rect-2i) (dst vector-2i))

(defgmethod (image+fill :class 'image :bind "fill" :hash 2920490490) :void
 (color color))

(defgmethod (image+fill-rect :class 'image :bind "fill_rect" :hash 514693913)
 :void (rect rect-2i) (color color))

(defgmethod
 (image+get-used-rect :class 'image :bind "get_used_rect" :hash 410525958)
 rect-2i)

(defgmethod
 (image+get-region :class 'image :bind "get_region" :hash 2601441065) image
 (region rect-2i))

(defgmethod (image+copy-from :class 'image :bind "copy_from" :hash 532598488)
 :void (src image))

(defgmethod
 (image+get-pixelv :class 'image :bind "get_pixelv" :hash 1532707496) color
 (point vector-2i))

(defgmethod (image+get-pixel :class 'image :bind "get_pixel" :hash 2165839948)
 color (x int) (y int))

(defgmethod (image+set-pixelv :class 'image :bind "set_pixelv" :hash 287851464)
 :void (point vector-2i) (color color))

(defgmethod (image+set-pixel :class 'image :bind "set_pixel" :hash 3733378741)
 :void (x int) (y int) (color color))

(defgmethod
 (image+adjust-bcs :class 'image :bind "adjust_bcs" :hash 2385087082) :void
 (brightness float) (contrast float) (saturation float))

(defgmethod
 (image+load-png-from-buffer :class 'image :bind "load_png_from_buffer" :hash
  680677267)
 error (buffer packed-byte-array))

(defgmethod
 (image+load-jpg-from-buffer :class 'image :bind "load_jpg_from_buffer" :hash
  680677267)
 error (buffer packed-byte-array))

(defgmethod
 (image+load-webp-from-buffer :class 'image :bind "load_webp_from_buffer" :hash
  680677267)
 error (buffer packed-byte-array))

(defgmethod
 (image+load-tga-from-buffer :class 'image :bind "load_tga_from_buffer" :hash
  680677267)
 error (buffer packed-byte-array))

(defgmethod
 (image+load-bmp-from-buffer :class 'image :bind "load_bmp_from_buffer" :hash
  680677267)
 error (buffer packed-byte-array))

(defgmethod
 (image+load-ktx-from-buffer :class 'image :bind "load_ktx_from_buffer" :hash
  680677267)
 error (buffer packed-byte-array))

(defgmethod
 (image+load-dds-from-buffer :class 'image :bind "load_dds_from_buffer" :hash
  680677267)
 error (buffer packed-byte-array))

(defgmethod
 (image+load-exr-from-buffer :class 'image :bind "load_exr_from_buffer" :hash
  680677267)
 error (buffer packed-byte-array))

(defgmethod
 (image+load-svg-from-buffer :class 'image :bind "load_svg_from_buffer" :hash
  311853421)
 error (buffer packed-byte-array) (scale float))

(defgmethod
 (image+load-svg-from-string :class 'image :bind "load_svg_from_string" :hash
  3254053600)
 error (svg-str string) (scale float))