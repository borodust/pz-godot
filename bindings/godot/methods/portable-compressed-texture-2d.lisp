(common-lisp:in-package :%godot)


(defgmethod
 (portable-compressed-texture-2d+create-from-image :class
  'portable-compressed-texture-2d :bind "create_from_image" :hash 3679243433)
 :void (image image)
 (compression-mode portable-compressed-texture-2d+compression-mode)
 (normal-map bool) (lossy-quality float))

(defgmethod
 (portable-compressed-texture-2d+get-format :class
  'portable-compressed-texture-2d :bind "get_format" :hash 3847873762)
 image+format)

(defgmethod
 (portable-compressed-texture-2d+get-compression-mode :class
  'portable-compressed-texture-2d :bind "get_compression_mode" :hash
  3265612739)
 portable-compressed-texture-2d+compression-mode)

(defgmethod
 (portable-compressed-texture-2d+set-size-override :class
  'portable-compressed-texture-2d :bind "set_size_override" :hash 743155724)
 :void (size vector-2))

(defgmethod
 (portable-compressed-texture-2d+get-size-override :class
  'portable-compressed-texture-2d :bind "get_size_override" :hash 3341600327)
 vector-2)

(defgmethod
 (portable-compressed-texture-2d+set-keep-compressed-buffer :class
  'portable-compressed-texture-2d :bind "set_keep_compressed_buffer" :hash
  2586408642)
 :void (keep bool))

(defgmethod
 (portable-compressed-texture-2d+is-keeping-compressed-buffer :class
  'portable-compressed-texture-2d :bind "is_keeping_compressed_buffer" :hash
  36873697)
 bool)

(defgmethod
 (portable-compressed-texture-2d+set-basisu-compressor-params :class
  'portable-compressed-texture-2d :bind "set_basisu_compressor_params" :hash
  1602489585)
 :void (uastc-level int) (rdo-quality-loss float))

(defgmethod
 (portable-compressed-texture-2d+set-keep-all-compressed-buffers :class
  'portable-compressed-texture-2d :bind "set_keep_all_compressed_buffers" :hash
  2586408642 :static common-lisp:t)
 :void (keep bool))

(defgmethod
 (portable-compressed-texture-2d+is-keeping-all-compressed-buffers :class
  'portable-compressed-texture-2d :bind "is_keeping_all_compressed_buffers"
  :hash 2240911060 :static common-lisp:t)
 bool)