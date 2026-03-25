(common-lisp:in-package :%godot)


(defgmethod
 (geometry-instance-3d+set-material-override :class 'geometry-instance-3d :bind
  "set_material_override" :hash 2757459619)
 :void (material material))

(defgmethod
 (geometry-instance-3d+get-material-override :class 'geometry-instance-3d :bind
  "get_material_override" :hash 5934680)
 material)

(defgmethod
 (geometry-instance-3d+set-material-overlay :class 'geometry-instance-3d :bind
  "set_material_overlay" :hash 2757459619)
 :void (material material))

(defgmethod
 (geometry-instance-3d+get-material-overlay :class 'geometry-instance-3d :bind
  "get_material_overlay" :hash 5934680)
 material)

(defgmethod
 (geometry-instance-3d+set-cast-shadows-setting :class 'geometry-instance-3d
  :bind "set_cast_shadows_setting" :hash 856677339)
 :void (shadow-casting-setting geometry-instance-3d+shadow-casting-setting))

(defgmethod
 (geometry-instance-3d+get-cast-shadows-setting :class 'geometry-instance-3d
  :bind "get_cast_shadows_setting" :hash 3383019359)
 geometry-instance-3d+shadow-casting-setting)

(defgmethod
 (geometry-instance-3d+set-lod-bias :class 'geometry-instance-3d :bind
  "set_lod_bias" :hash 373806689)
 :void (bias float))

(defgmethod
 (geometry-instance-3d+get-lod-bias :class 'geometry-instance-3d :bind
  "get_lod_bias" :hash 1740695150)
 float)

(defgmethod
 (geometry-instance-3d+set-transparency :class 'geometry-instance-3d :bind
  "set_transparency" :hash 373806689)
 :void (transparency float))

(defgmethod
 (geometry-instance-3d+get-transparency :class 'geometry-instance-3d :bind
  "get_transparency" :hash 1740695150)
 float)

(defgmethod
 (geometry-instance-3d+set-visibility-range-end-margin :class
  'geometry-instance-3d :bind "set_visibility_range_end_margin" :hash
  373806689)
 :void (distance float))

(defgmethod
 (geometry-instance-3d+get-visibility-range-end-margin :class
  'geometry-instance-3d :bind "get_visibility_range_end_margin" :hash
  1740695150)
 float)

(defgmethod
 (geometry-instance-3d+set-visibility-range-end :class 'geometry-instance-3d
  :bind "set_visibility_range_end" :hash 373806689)
 :void (distance float))

(defgmethod
 (geometry-instance-3d+get-visibility-range-end :class 'geometry-instance-3d
  :bind "get_visibility_range_end" :hash 1740695150)
 float)

(defgmethod
 (geometry-instance-3d+set-visibility-range-begin-margin :class
  'geometry-instance-3d :bind "set_visibility_range_begin_margin" :hash
  373806689)
 :void (distance float))

(defgmethod
 (geometry-instance-3d+get-visibility-range-begin-margin :class
  'geometry-instance-3d :bind "get_visibility_range_begin_margin" :hash
  1740695150)
 float)

(defgmethod
 (geometry-instance-3d+set-visibility-range-begin :class 'geometry-instance-3d
  :bind "set_visibility_range_begin" :hash 373806689)
 :void (distance float))

(defgmethod
 (geometry-instance-3d+get-visibility-range-begin :class 'geometry-instance-3d
  :bind "get_visibility_range_begin" :hash 1740695150)
 float)

(defgmethod
 (geometry-instance-3d+set-visibility-range-fade-mode :class
  'geometry-instance-3d :bind "set_visibility_range_fade_mode" :hash
  1440117808)
 :void (mode geometry-instance-3d+visibility-range-fade-mode))

(defgmethod
 (geometry-instance-3d+get-visibility-range-fade-mode :class
  'geometry-instance-3d :bind "get_visibility_range_fade_mode" :hash
  2067221882)
 geometry-instance-3d+visibility-range-fade-mode)

(defgmethod
 (geometry-instance-3d+set-instance-shader-parameter :class
  'geometry-instance-3d :bind "set_instance_shader_parameter" :hash 3776071444)
 :void (name string-name) (value variant))

(defgmethod
 (geometry-instance-3d+get-instance-shader-parameter :class
  'geometry-instance-3d :bind "get_instance_shader_parameter" :hash 2760726917)
 variant (name string-name))

(defgmethod
 (geometry-instance-3d+set-extra-cull-margin :class 'geometry-instance-3d :bind
  "set_extra_cull_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (geometry-instance-3d+get-extra-cull-margin :class 'geometry-instance-3d :bind
  "get_extra_cull_margin" :hash 1740695150)
 float)

(defgmethod
 (geometry-instance-3d+set-lightmap-texel-scale :class 'geometry-instance-3d
  :bind "set_lightmap_texel_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (geometry-instance-3d+get-lightmap-texel-scale :class 'geometry-instance-3d
  :bind "get_lightmap_texel_scale" :hash 1740695150)
 float)

(defgmethod
 (geometry-instance-3d+set-lightmap-scale :class 'geometry-instance-3d :bind
  "set_lightmap_scale" :hash 2462696582)
 :void (scale geometry-instance-3d+lightmap-scale))

(defgmethod
 (geometry-instance-3d+get-lightmap-scale :class 'geometry-instance-3d :bind
  "get_lightmap_scale" :hash 798767852)
 geometry-instance-3d+lightmap-scale)

(defgmethod
 (geometry-instance-3d+set-gi-mode :class 'geometry-instance-3d :bind
  "set_gi_mode" :hash 2548557163)
 :void (mode geometry-instance-3d+gimode))

(defgmethod
 (geometry-instance-3d+get-gi-mode :class 'geometry-instance-3d :bind
  "get_gi_mode" :hash 2188566509)
 geometry-instance-3d+gimode)

(defgmethod
 (geometry-instance-3d+set-ignore-occlusion-culling :class
  'geometry-instance-3d :bind "set_ignore_occlusion_culling" :hash 2586408642)
 :void (ignore-culling bool))

(defgmethod
 (geometry-instance-3d+is-ignoring-occlusion-culling :class
  'geometry-instance-3d :bind "is_ignoring_occlusion_culling" :hash 2240911060)
 bool)

(defgmethod
 (geometry-instance-3d+set-custom-aabb :class 'geometry-instance-3d :bind
  "set_custom_aabb" :hash 259215842)
 :void (aabb aabb))

(defgmethod
 (geometry-instance-3d+get-custom-aabb :class 'geometry-instance-3d :bind
  "get_custom_aabb" :hash 1068685055)
 aabb)