(common-lisp:in-package :%godot)


(defgmethod
 (drawable-texture-2d+set-format :class 'drawable-texture-2d :bind "set_format"
  :hash 2875673594)
 :void (format drawable-texture-2d+drawable-format))

(defgmethod
 (drawable-texture-2d+set-use-mipmaps :class 'drawable-texture-2d :bind
  "set_use_mipmaps" :hash 2586408642)
 :void (mipmaps bool))

(defgmethod
 (drawable-texture-2d+get-use-mipmaps :class 'drawable-texture-2d :bind
  "get_use_mipmaps" :hash 36873697)
 bool)

(defgmethod
 (drawable-texture-2d+setup :class 'drawable-texture-2d :bind "setup" :hash
  674365339)
 :void (width int) (height int) (format drawable-texture-2d+drawable-format)
 (color color) (use-mipmaps bool))

(defgmethod
 (drawable-texture-2d+blit-rect :class 'drawable-texture-2d :bind "blit_rect"
  :hash 319217173)
 :void (rect rect-2i) (source texture-2d) (modulate color) (mipmap int)
 (material material))

(defgmethod
 (drawable-texture-2d+blit-rect-multi :class 'drawable-texture-2d :bind
  "blit_rect_multi" :hash 3074783066)
 :void (rect rect-2i) (sources array) (extra-targets array) (modulate color)
 (mipmap int) (material material))

(defgmethod
 (drawable-texture-2d+generate-mipmaps :class 'drawable-texture-2d :bind
  "generate_mipmaps" :hash 3218959716)
 :void)