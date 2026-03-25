(common-lisp:in-package :%godot)


(defgmethod
 (render-scene-buffers-extension+-configure :class
  'render-scene-buffers-extension :bind "_configure" :hash 3072623270 :virtual
  common-lisp:t)
 :void (config render-scene-buffers-configuration))

(defgmethod
 (render-scene-buffers-extension+-set-fsr-sharpness :class
  'render-scene-buffers-extension :bind "_set_fsr_sharpness" :hash 373806689
  :virtual common-lisp:t)
 :void (fsr-sharpness float))

(defgmethod
 (render-scene-buffers-extension+-set-texture-mipmap-bias :class
  'render-scene-buffers-extension :bind "_set_texture_mipmap_bias" :hash
  373806689 :virtual common-lisp:t)
 :void (texture-mipmap-bias float))

(defgmethod
 (render-scene-buffers-extension+-set-anisotropic-filtering-level :class
  'render-scene-buffers-extension :bind "_set_anisotropic_filtering_level"
  :hash 1286410249 :virtual common-lisp:t)
 :void (anisotropic-filtering-level int))

(defgmethod
 (render-scene-buffers-extension+-set-use-debanding :class
  'render-scene-buffers-extension :bind "_set_use_debanding" :hash 2586408642
  :virtual common-lisp:t)
 :void (use-debanding bool))