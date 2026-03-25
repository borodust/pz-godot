(common-lisp:in-package :%godot)


(defgmethod
 (render-scene-buffers-rd+has-texture :class 'render-scene-buffers-rd :bind
  "has_texture" :hash 471820014)
 bool (context string-name) (name string-name))

(defgmethod
 (render-scene-buffers-rd+create-texture :class 'render-scene-buffers-rd :bind
  "create_texture" :hash 2950875024)
 rid (context string-name) (name string-name)
 (data-format rendering-device+data-format) (usage-bits int)
 (texture-samples rendering-device+texture-samples) (size vector-2i)
 (layers int) (mipmaps int) (unique bool) (discardable bool))

(defgmethod
 (render-scene-buffers-rd+create-texture-from-format :class
  'render-scene-buffers-rd :bind "create_texture_from_format" :hash 3344669382)
 rid (context string-name) (name string-name) (format rdtexture-format)
 (view rdtexture-view) (unique bool))

(defgmethod
 (render-scene-buffers-rd+create-texture-view :class 'render-scene-buffers-rd
  :bind "create_texture_view" :hash 283055834)
 rid (context string-name) (name string-name) (view-name string-name)
 (view rdtexture-view))

(defgmethod
 (render-scene-buffers-rd+get-texture :class 'render-scene-buffers-rd :bind
  "get_texture" :hash 750006389)
 rid (context string-name) (name string-name))

(defgmethod
 (render-scene-buffers-rd+get-texture-format :class 'render-scene-buffers-rd
  :bind "get_texture_format" :hash 371461758)
 rdtexture-format (context string-name) (name string-name))

(defgmethod
 (render-scene-buffers-rd+get-texture-slice :class 'render-scene-buffers-rd
  :bind "get_texture_slice" :hash 588440706)
 rid (context string-name) (name string-name) (layer int) (mipmap int)
 (layers int) (mipmaps int))

(defgmethod
 (render-scene-buffers-rd+get-texture-slice-view :class
  'render-scene-buffers-rd :bind "get_texture_slice_view" :hash 682451778)
 rid (context string-name) (name string-name) (layer int) (mipmap int)
 (layers int) (mipmaps int) (view rdtexture-view))

(defgmethod
 (render-scene-buffers-rd+get-texture-slice-size :class
  'render-scene-buffers-rd :bind "get_texture_slice_size" :hash 2617625368)
 vector-2i (context string-name) (name string-name) (mipmap int))

(defgmethod
 (render-scene-buffers-rd+clear-context :class 'render-scene-buffers-rd :bind
  "clear_context" :hash 3304788590)
 :void (context string-name))

(defgmethod
 (render-scene-buffers-rd+get-color-texture :class 'render-scene-buffers-rd
  :bind "get_color_texture" :hash 3050822880)
 rid (msaa bool))

(defgmethod
 (render-scene-buffers-rd+get-color-layer :class 'render-scene-buffers-rd :bind
  "get_color_layer" :hash 3087988589)
 rid (layer int) (msaa bool))

(defgmethod
 (render-scene-buffers-rd+get-depth-texture :class 'render-scene-buffers-rd
  :bind "get_depth_texture" :hash 3050822880)
 rid (msaa bool))

(defgmethod
 (render-scene-buffers-rd+get-depth-layer :class 'render-scene-buffers-rd :bind
  "get_depth_layer" :hash 3087988589)
 rid (layer int) (msaa bool))

(defgmethod
 (render-scene-buffers-rd+get-velocity-texture :class 'render-scene-buffers-rd
  :bind "get_velocity_texture" :hash 3050822880)
 rid (msaa bool))

(defgmethod
 (render-scene-buffers-rd+get-velocity-layer :class 'render-scene-buffers-rd
  :bind "get_velocity_layer" :hash 3087988589)
 rid (layer int) (msaa bool))

(defgmethod
 (render-scene-buffers-rd+get-render-target :class 'render-scene-buffers-rd
  :bind "get_render_target" :hash 2944877500)
 rid)

(defgmethod
 (render-scene-buffers-rd+get-view-count :class 'render-scene-buffers-rd :bind
  "get_view_count" :hash 3905245786)
 int)

(defgmethod
 (render-scene-buffers-rd+get-internal-size :class 'render-scene-buffers-rd
  :bind "get_internal_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (render-scene-buffers-rd+get-target-size :class 'render-scene-buffers-rd :bind
  "get_target_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (render-scene-buffers-rd+get-scaling-3d-mode :class 'render-scene-buffers-rd
  :bind "get_scaling_3d_mode" :hash 976778074)
 rendering-server+viewport-scaling-3dmode)

(defgmethod
 (render-scene-buffers-rd+get-fsr-sharpness :class 'render-scene-buffers-rd
  :bind "get_fsr_sharpness" :hash 1740695150)
 float)

(defgmethod
 (render-scene-buffers-rd+get-msaa-3d :class 'render-scene-buffers-rd :bind
  "get_msaa_3d" :hash 3109158617)
 rendering-server+viewport-msaa)

(defgmethod
 (render-scene-buffers-rd+get-texture-samples :class 'render-scene-buffers-rd
  :bind "get_texture_samples" :hash 407791724)
 rendering-device+texture-samples)

(defgmethod
 (render-scene-buffers-rd+get-screen-space-aa :class 'render-scene-buffers-rd
  :bind "get_screen_space_aa" :hash 641513172)
 rendering-server+viewport-screen-space-aa)

(defgmethod
 (render-scene-buffers-rd+get-use-taa :class 'render-scene-buffers-rd :bind
  "get_use_taa" :hash 36873697)
 bool)

(defgmethod
 (render-scene-buffers-rd+get-use-debanding :class 'render-scene-buffers-rd
  :bind "get_use_debanding" :hash 36873697)
 bool)