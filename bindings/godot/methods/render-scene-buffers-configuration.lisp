(common-lisp:in-package :%godot)


(defgmethod
 (render-scene-buffers-configuration+get-render-target :class
  'render-scene-buffers-configuration :bind "get_render_target" :hash
  2944877500)
 rid)

(defgmethod
 (render-scene-buffers-configuration+set-render-target :class
  'render-scene-buffers-configuration :bind "set_render_target" :hash
  2722037293)
 :void (render-target rid))

(defgmethod
 (render-scene-buffers-configuration+get-internal-size :class
  'render-scene-buffers-configuration :bind "get_internal_size" :hash
  3690982128)
 vector-2i)

(defgmethod
 (render-scene-buffers-configuration+set-internal-size :class
  'render-scene-buffers-configuration :bind "set_internal_size" :hash
  1130785943)
 :void (internal-size vector-2i))

(defgmethod
 (render-scene-buffers-configuration+get-target-size :class
  'render-scene-buffers-configuration :bind "get_target_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (render-scene-buffers-configuration+set-target-size :class
  'render-scene-buffers-configuration :bind "set_target_size" :hash 1130785943)
 :void (target-size vector-2i))

(defgmethod
 (render-scene-buffers-configuration+get-view-count :class
  'render-scene-buffers-configuration :bind "get_view_count" :hash 3905245786)
 int)

(defgmethod
 (render-scene-buffers-configuration+set-view-count :class
  'render-scene-buffers-configuration :bind "set_view_count" :hash 1286410249)
 :void (view-count int))

(defgmethod
 (render-scene-buffers-configuration+get-scaling-3d-mode :class
  'render-scene-buffers-configuration :bind "get_scaling_3d_mode" :hash
  976778074)
 rendering-server+viewport-scaling-3dmode)

(defgmethod
 (render-scene-buffers-configuration+set-scaling-3d-mode :class
  'render-scene-buffers-configuration :bind "set_scaling_3d_mode" :hash
  447477857)
 :void (scaling-3d-mode rendering-server+viewport-scaling-3dmode))

(defgmethod
 (render-scene-buffers-configuration+get-msaa-3d :class
  'render-scene-buffers-configuration :bind "get_msaa_3d" :hash 3109158617)
 rendering-server+viewport-msaa)

(defgmethod
 (render-scene-buffers-configuration+set-msaa-3d :class
  'render-scene-buffers-configuration :bind "set_msaa_3d" :hash 3952630748)
 :void (msaa-3d rendering-server+viewport-msaa))

(defgmethod
 (render-scene-buffers-configuration+get-screen-space-aa :class
  'render-scene-buffers-configuration :bind "get_screen_space_aa" :hash
  641513172)
 rendering-server+viewport-screen-space-aa)

(defgmethod
 (render-scene-buffers-configuration+set-screen-space-aa :class
  'render-scene-buffers-configuration :bind "set_screen_space_aa" :hash
  139543108)
 :void (screen-space-aa rendering-server+viewport-screen-space-aa))

(defgmethod
 (render-scene-buffers-configuration+get-fsr-sharpness :class
  'render-scene-buffers-configuration :bind "get_fsr_sharpness" :hash
  1740695150)
 float)

(defgmethod
 (render-scene-buffers-configuration+set-fsr-sharpness :class
  'render-scene-buffers-configuration :bind "set_fsr_sharpness" :hash
  373806689)
 :void (fsr-sharpness float))

(defgmethod
 (render-scene-buffers-configuration+get-texture-mipmap-bias :class
  'render-scene-buffers-configuration :bind "get_texture_mipmap_bias" :hash
  1740695150)
 float)

(defgmethod
 (render-scene-buffers-configuration+set-texture-mipmap-bias :class
  'render-scene-buffers-configuration :bind "set_texture_mipmap_bias" :hash
  373806689)
 :void (texture-mipmap-bias float))

(defgmethod
 (render-scene-buffers-configuration+get-anisotropic-filtering-level :class
  'render-scene-buffers-configuration :bind "get_anisotropic_filtering_level"
  :hash 1617414954)
 rendering-server+viewport-anisotropic-filtering)

(defgmethod
 (render-scene-buffers-configuration+set-anisotropic-filtering-level :class
  'render-scene-buffers-configuration :bind "set_anisotropic_filtering_level"
  :hash 2559658741)
 :void
 (anisotropic-filtering-level rendering-server+viewport-anisotropic-filtering))