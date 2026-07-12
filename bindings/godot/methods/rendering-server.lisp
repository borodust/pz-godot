(common-lisp:in-package :%godot)


(defgmethod
 (rendering-server+texture-2d-create :class 'rendering-server :bind
  "texture_2d_create" :hash 2010018390)
 rid (image image))

(defgmethod
 (rendering-server+texture-2d-layered-create :class 'rendering-server :bind
  "texture_2d_layered_create" :hash 913689023)
 rid (layers array) (layered-type rendering-server+texture-layered-type))

(defgmethod
 (rendering-server+texture-3d-create :class 'rendering-server :bind
  "texture_3d_create" :hash 4036838706)
 rid (format image+format) (width int) (height int) (depth int) (mipmaps bool)
 (data array))

(defgmethod
 (rendering-server+texture-proxy-create :class 'rendering-server :bind
  "texture_proxy_create" :hash 41030802)
 rid (base rid))

(defgmethod
 (rendering-server+texture-create-from-native-handle :class 'rendering-server
  :bind "texture_create_from_native_handle" :hash 1682977582)
 rid (type rendering-server+texture-type) (format image+format)
 (native-handle int) (width int) (height int) (depth int) (layers int)
 (layered-type rendering-server+texture-layered-type))

(defgmethod
 (rendering-server+texture-drawable-create :class 'rendering-server :bind
  "texture_drawable_create" :hash 1993613667)
 rid (width int) (height int) (format rendering-server+texture-drawable-format)
 (color color) (with-mipmaps bool))

(defgmethod
 (rendering-server+texture-2d-update :class 'rendering-server :bind
  "texture_2d_update" :hash 999539803)
 :void (texture rid) (image image) (layer int))

(defgmethod
 (rendering-server+texture-3d-update :class 'rendering-server :bind
  "texture_3d_update" :hash 684822712)
 :void (texture rid) (data array))

(defgmethod
 (rendering-server+texture-proxy-update :class 'rendering-server :bind
  "texture_proxy_update" :hash 395945892)
 :void (texture rid) (proxy-to rid))

(defgmethod
 (rendering-server+texture-drawable-blit-rect :class 'rendering-server :bind
  "texture_drawable_blit_rect" :hash 4077763890)
 :void (textures array) (rect rect-2i) (material rid) (modulate color)
 (source-textures array) (to-mipmap int))

(defgmethod
 (rendering-server+texture-2d-placeholder-create :class 'rendering-server :bind
  "texture_2d_placeholder_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+texture-2d-layered-placeholder-create :class
  'rendering-server :bind "texture_2d_layered_placeholder_create" :hash
  1394585590)
 rid (layered-type rendering-server+texture-layered-type))

(defgmethod
 (rendering-server+texture-3d-placeholder-create :class 'rendering-server :bind
  "texture_3d_placeholder_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+texture-2d-get :class 'rendering-server :bind
  "texture_2d_get" :hash 4206205781)
 image (texture rid))

(defgmethod
 (rendering-server+texture-2d-layer-get :class 'rendering-server :bind
  "texture_2d_layer_get" :hash 2705440895)
 image (texture rid) (layer int))

(defgmethod
 (rendering-server+texture-3d-get :class 'rendering-server :bind
  "texture_3d_get" :hash 2684255073)
 array (texture rid))

(defgmethod
 (rendering-server+texture-drawable-generate-mipmaps :class 'rendering-server
  :bind "texture_drawable_generate_mipmaps" :hash 2722037293)
 :void (texture rid))

(defgmethod
 (rendering-server+texture-drawable-get-default-material :class
  'rendering-server :bind "texture_drawable_get_default_material" :hash
  2944877500)
 rid)

(defgmethod
 (rendering-server+texture-replace :class 'rendering-server :bind
  "texture_replace" :hash 395945892)
 :void (texture rid) (by-texture rid))

(defgmethod
 (rendering-server+texture-set-size-override :class 'rendering-server :bind
  "texture_set_size_override" :hash 4288446313)
 :void (texture rid) (width int) (height int))

(defgmethod
 (rendering-server+texture-set-path :class 'rendering-server :bind
  "texture_set_path" :hash 2726140452)
 :void (texture rid) (path string))

(defgmethod
 (rendering-server+texture-get-path :class 'rendering-server :bind
  "texture_get_path" :hash 642473191)
 string (texture rid))

(defgmethod
 (rendering-server+texture-get-format :class 'rendering-server :bind
  "texture_get_format" :hash 1932918979)
 image+format (texture rid))

(defgmethod
 (rendering-server+texture-set-force-redraw-if-visible :class 'rendering-server
  :bind "texture_set_force_redraw_if_visible" :hash 1265174801)
 :void (texture rid) (enable bool))

(defgmethod
 (rendering-server+texture-rd-create :class 'rendering-server :bind
  "texture_rd_create" :hash 1434128712)
 rid (rd-texture rid) (layer-type rendering-server+texture-layered-type))

(defgmethod
 (rendering-server+texture-get-rd-texture :class 'rendering-server :bind
  "texture_get_rd_texture" :hash 2790148051)
 rid (texture rid) (srgb bool))

(defgmethod
 (rendering-server+texture-get-native-handle :class 'rendering-server :bind
  "texture_get_native_handle" :hash 1834114100)
 int (texture rid) (srgb bool))

(defgmethod
 (rendering-server+shader-create :class 'rendering-server :bind "shader_create"
  :hash 529393457)
 rid)

(defgmethod
 (rendering-server+shader-set-code :class 'rendering-server :bind
  "shader_set_code" :hash 2726140452)
 :void (shader rid) (code string))

(defgmethod
 (rendering-server+shader-set-path-hint :class 'rendering-server :bind
  "shader_set_path_hint" :hash 2726140452)
 :void (shader rid) (path string))

(defgmethod
 (rendering-server+shader-get-code :class 'rendering-server :bind
  "shader_get_code" :hash 642473191)
 string (shader rid))

(defgmethod
 (rendering-server+get-shader-parameter-list :class 'rendering-server :bind
  "get_shader_parameter_list" :hash 2684255073)
 array (shader rid))

(defgmethod
 (rendering-server+shader-get-parameter-default :class 'rendering-server :bind
  "shader_get_parameter_default" :hash 2621281810)
 variant (shader rid) (name string-name))

(defgmethod
 (rendering-server+shader-set-default-texture-parameter :class
  'rendering-server :bind "shader_set_default_texture_parameter" :hash
  4094001817)
 :void (shader rid) (name string-name) (texture rid) (index int))

(defgmethod
 (rendering-server+shader-get-default-texture-parameter :class
  'rendering-server :bind "shader_get_default_texture_parameter" :hash
  1464608890)
 rid (shader rid) (name string-name) (index int))

(defgmethod
 (rendering-server+material-create :class 'rendering-server :bind
  "material_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+material-set-shader :class 'rendering-server :bind
  "material_set_shader" :hash 395945892)
 :void (shader-material rid) (shader rid))

(defgmethod
 (rendering-server+material-set-param :class 'rendering-server :bind
  "material_set_param" :hash 3477296213)
 :void (material rid) (parameter string-name) (value variant))

(defgmethod
 (rendering-server+material-get-param :class 'rendering-server :bind
  "material_get_param" :hash 2621281810)
 variant (material rid) (parameter string-name))

(defgmethod
 (rendering-server+material-set-render-priority :class 'rendering-server :bind
  "material_set_render_priority" :hash 3411492887)
 :void (material rid) (priority int))

(defgmethod
 (rendering-server+material-set-next-pass :class 'rendering-server :bind
  "material_set_next_pass" :hash 395945892)
 :void (material rid) (next-material rid))

(defgmethod
 (rendering-server+material-set-use-debanding :class 'rendering-server :bind
  "material_set_use_debanding" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (rendering-server+mesh-create-from-surfaces :class 'rendering-server :bind
  "mesh_create_from_surfaces" :hash 4291747531)
 rid (surfaces array) (blend-shape-count int))

(defgmethod
 (rendering-server+mesh-create :class 'rendering-server :bind "mesh_create"
  :hash 529393457)
 rid)

(defgmethod
 (rendering-server+mesh-surface-get-format-offset :class 'rendering-server
  :bind "mesh_surface_get_format_offset" :hash 2981368685)
 int (format rendering-server+array-format) (vertex-count int)
 (array-index int))

(defgmethod
 (rendering-server+mesh-surface-get-format-vertex-stride :class
  'rendering-server :bind "mesh_surface_get_format_vertex_stride" :hash
  3188363337)
 int (format rendering-server+array-format) (vertex-count int))

(defgmethod
 (rendering-server+mesh-surface-get-format-normal-tangent-stride :class
  'rendering-server :bind "mesh_surface_get_format_normal_tangent_stride" :hash
  3188363337)
 int (format rendering-server+array-format) (vertex-count int))

(defgmethod
 (rendering-server+mesh-surface-get-format-attribute-stride :class
  'rendering-server :bind "mesh_surface_get_format_attribute_stride" :hash
  3188363337)
 int (format rendering-server+array-format) (vertex-count int))

(defgmethod
 (rendering-server+mesh-surface-get-format-skin-stride :class 'rendering-server
  :bind "mesh_surface_get_format_skin_stride" :hash 3188363337)
 int (format rendering-server+array-format) (vertex-count int))

(defgmethod
 (rendering-server+mesh-surface-get-format-index-stride :class
  'rendering-server :bind "mesh_surface_get_format_index_stride" :hash
  3188363337)
 int (format rendering-server+array-format) (vertex-count int))

(defgmethod
 (rendering-server+mesh-add-surface :class 'rendering-server :bind
  "mesh_add_surface" :hash 1217542888)
 :void (mesh rid) (surface dictionary))

(defgmethod
 (rendering-server+mesh-add-surface-from-arrays :class 'rendering-server :bind
  "mesh_add_surface_from_arrays" :hash 2342446560)
 :void (mesh rid) (primitive rendering-server+primitive-type) (arrays array)
 (blend-shapes array) (lods dictionary)
 (compress-format rendering-server+array-format))

(defgmethod
 (rendering-server+mesh-get-blend-shape-count :class 'rendering-server :bind
  "mesh_get_blend_shape_count" :hash 2198884583)
 int (mesh rid))

(defgmethod
 (rendering-server+mesh-set-blend-shape-mode :class 'rendering-server :bind
  "mesh_set_blend_shape_mode" :hash 1294662092)
 :void (mesh rid) (mode rendering-server+blend-shape-mode))

(defgmethod
 (rendering-server+mesh-get-blend-shape-mode :class 'rendering-server :bind
  "mesh_get_blend_shape_mode" :hash 4282291819)
 rendering-server+blend-shape-mode (mesh rid))

(defgmethod
 (rendering-server+mesh-surface-set-material :class 'rendering-server :bind
  "mesh_surface_set_material" :hash 2310537182)
 :void (mesh rid) (surface int) (material rid))

(defgmethod
 (rendering-server+mesh-surface-get-material :class 'rendering-server :bind
  "mesh_surface_get_material" :hash 1066463050)
 rid (mesh rid) (surface int))

(defgmethod
 (rendering-server+mesh-get-surface :class 'rendering-server :bind
  "mesh_get_surface" :hash 186674697)
 dictionary (mesh rid) (surface int))

(defgmethod
 (rendering-server+mesh-surface-get-arrays :class 'rendering-server :bind
  "mesh_surface_get_arrays" :hash 1778388067)
 array (mesh rid) (surface int))

(defgmethod
 (rendering-server+mesh-surface-get-blend-shape-arrays :class 'rendering-server
  :bind "mesh_surface_get_blend_shape_arrays" :hash 1778388067)
 array (mesh rid) (surface int))

(defgmethod
 (rendering-server+mesh-get-surface-count :class 'rendering-server :bind
  "mesh_get_surface_count" :hash 2198884583)
 int (mesh rid))

(defgmethod
 (rendering-server+mesh-set-custom-aabb :class 'rendering-server :bind
  "mesh_set_custom_aabb" :hash 3696536120)
 :void (mesh rid) (aabb aabb))

(defgmethod
 (rendering-server+mesh-get-custom-aabb :class 'rendering-server :bind
  "mesh_get_custom_aabb" :hash 974181306)
 aabb (mesh rid))

(defgmethod
 (rendering-server+mesh-surface-remove :class 'rendering-server :bind
  "mesh_surface_remove" :hash 3411492887)
 :void (mesh rid) (surface int))

(defgmethod
 (rendering-server+mesh-clear :class 'rendering-server :bind "mesh_clear" :hash
  2722037293)
 :void (mesh rid))

(defgmethod
 (rendering-server+mesh-surface-update-vertex-region :class 'rendering-server
  :bind "mesh_surface_update_vertex_region" :hash 2900195149)
 :void (mesh rid) (surface int) (offset int) (data packed-byte-array))

(defgmethod
 (rendering-server+mesh-surface-update-attribute-region :class
  'rendering-server :bind "mesh_surface_update_attribute_region" :hash
  2900195149)
 :void (mesh rid) (surface int) (offset int) (data packed-byte-array))

(defgmethod
 (rendering-server+mesh-surface-update-skin-region :class 'rendering-server
  :bind "mesh_surface_update_skin_region" :hash 2900195149)
 :void (mesh rid) (surface int) (offset int) (data packed-byte-array))

(defgmethod
 (rendering-server+mesh-surface-update-index-region :class 'rendering-server
  :bind "mesh_surface_update_index_region" :hash 2900195149)
 :void (mesh rid) (surface int) (offset int) (data packed-byte-array))

(defgmethod
 (rendering-server+mesh-set-shadow-mesh :class 'rendering-server :bind
  "mesh_set_shadow_mesh" :hash 395945892)
 :void (mesh rid) (shadow-mesh rid))

(defgmethod
 (rendering-server+multimesh-create :class 'rendering-server :bind
  "multimesh_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+multimesh-allocate-data :class 'rendering-server :bind
  "multimesh_allocate_data" :hash 557240154)
 :void (multimesh rid) (instances int)
 (transform-format rendering-server+multimesh-transform-format)
 (color-format bool) (custom-data-format bool) (use-indirect bool))

(defgmethod
 (rendering-server+multimesh-get-instance-count :class 'rendering-server :bind
  "multimesh_get_instance_count" :hash 2198884583)
 int (multimesh rid))

(defgmethod
 (rendering-server+multimesh-set-mesh :class 'rendering-server :bind
  "multimesh_set_mesh" :hash 395945892)
 :void (multimesh rid) (mesh rid))

(defgmethod
 (rendering-server+multimesh-instance-set-transform :class 'rendering-server
  :bind "multimesh_instance_set_transform" :hash 675327471)
 :void (multimesh rid) (index int) (transform transform-3d))

(defgmethod
 (rendering-server+multimesh-instance-set-transform-2d :class 'rendering-server
  :bind "multimesh_instance_set_transform_2d" :hash 736082694)
 :void (multimesh rid) (index int) (transform transform-2d))

(defgmethod
 (rendering-server+multimesh-instance-set-color :class 'rendering-server :bind
  "multimesh_instance_set_color" :hash 176975443)
 :void (multimesh rid) (index int) (color color))

(defgmethod
 (rendering-server+multimesh-instance-set-custom-data :class 'rendering-server
  :bind "multimesh_instance_set_custom_data" :hash 176975443)
 :void (multimesh rid) (index int) (custom-data color))

(defgmethod
 (rendering-server+multimesh-get-mesh :class 'rendering-server :bind
  "multimesh_get_mesh" :hash 3814569979)
 rid (multimesh rid))

(defgmethod
 (rendering-server+multimesh-get-aabb :class 'rendering-server :bind
  "multimesh_get_aabb" :hash 974181306)
 aabb (multimesh rid))

(defgmethod
 (rendering-server+multimesh-set-custom-aabb :class 'rendering-server :bind
  "multimesh_set_custom_aabb" :hash 3696536120)
 :void (multimesh rid) (aabb aabb))

(defgmethod
 (rendering-server+multimesh-get-custom-aabb :class 'rendering-server :bind
  "multimesh_get_custom_aabb" :hash 974181306)
 aabb (multimesh rid))

(defgmethod
 (rendering-server+multimesh-instance-get-transform :class 'rendering-server
  :bind "multimesh_instance_get_transform" :hash 1050775521)
 transform-3d (multimesh rid) (index int))

(defgmethod
 (rendering-server+multimesh-instance-get-transform-2d :class 'rendering-server
  :bind "multimesh_instance_get_transform_2d" :hash 1324854622)
 transform-2d (multimesh rid) (index int))

(defgmethod
 (rendering-server+multimesh-instance-get-color :class 'rendering-server :bind
  "multimesh_instance_get_color" :hash 2946315076)
 color (multimesh rid) (index int))

(defgmethod
 (rendering-server+multimesh-instance-get-custom-data :class 'rendering-server
  :bind "multimesh_instance_get_custom_data" :hash 2946315076)
 color (multimesh rid) (index int))

(defgmethod
 (rendering-server+multimesh-set-visible-instances :class 'rendering-server
  :bind "multimesh_set_visible_instances" :hash 3411492887)
 :void (multimesh rid) (visible int))

(defgmethod
 (rendering-server+multimesh-get-visible-instances :class 'rendering-server
  :bind "multimesh_get_visible_instances" :hash 2198884583)
 int (multimesh rid))

(defgmethod
 (rendering-server+multimesh-set-buffer :class 'rendering-server :bind
  "multimesh_set_buffer" :hash 2960552364)
 :void (multimesh rid) (buffer packed-float-32array))

(defgmethod
 (rendering-server+multimesh-get-command-buffer-rd-rid :class 'rendering-server
  :bind "multimesh_get_command_buffer_rd_rid" :hash 3814569979)
 rid (multimesh rid))

(defgmethod
 (rendering-server+multimesh-get-buffer-rd-rid :class 'rendering-server :bind
  "multimesh_get_buffer_rd_rid" :hash 3814569979)
 rid (multimesh rid))

(defgmethod
 (rendering-server+multimesh-get-buffer :class 'rendering-server :bind
  "multimesh_get_buffer" :hash 3964669176)
 packed-float-32array (multimesh rid))

(defgmethod
 (rendering-server+multimesh-set-buffer-interpolated :class 'rendering-server
  :bind "multimesh_set_buffer_interpolated" :hash 659844711)
 :void (multimesh rid) (buffer packed-float-32array)
 (buffer-previous packed-float-32array))

(defgmethod
 (rendering-server+multimesh-set-physics-interpolated :class 'rendering-server
  :bind "multimesh_set_physics_interpolated" :hash 1265174801)
 :void (multimesh rid) (interpolated bool))

(defgmethod
 (rendering-server+multimesh-set-physics-interpolation-quality :class
  'rendering-server :bind "multimesh_set_physics_interpolation_quality" :hash
  3934808223)
 :void (multimesh rid)
 (quality rendering-server+multimesh-physics-interpolation-quality))

(defgmethod
 (rendering-server+multimesh-instance-reset-physics-interpolation :class
  'rendering-server :bind "multimesh_instance_reset_physics_interpolation"
  :hash 3411492887)
 :void (multimesh rid) (index int))

(defgmethod
 (rendering-server+multimesh-instances-reset-physics-interpolation :class
  'rendering-server :bind "multimesh_instances_reset_physics_interpolation"
  :hash 2722037293)
 :void (multimesh rid))

(defgmethod
 (rendering-server+skeleton-create :class 'rendering-server :bind
  "skeleton_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+skeleton-allocate-data :class 'rendering-server :bind
  "skeleton_allocate_data" :hash 1904426712)
 :void (skeleton rid) (bones int) (is-2d-skeleton bool))

(defgmethod
 (rendering-server+skeleton-get-bone-count :class 'rendering-server :bind
  "skeleton_get_bone_count" :hash 2198884583)
 int (skeleton rid))

(defgmethod
 (rendering-server+skeleton-bone-set-transform :class 'rendering-server :bind
  "skeleton_bone_set_transform" :hash 675327471)
 :void (skeleton rid) (bone int) (transform transform-3d))

(defgmethod
 (rendering-server+skeleton-bone-get-transform :class 'rendering-server :bind
  "skeleton_bone_get_transform" :hash 1050775521)
 transform-3d (skeleton rid) (bone int))

(defgmethod
 (rendering-server+skeleton-bone-set-transform-2d :class 'rendering-server
  :bind "skeleton_bone_set_transform_2d" :hash 736082694)
 :void (skeleton rid) (bone int) (transform transform-2d))

(defgmethod
 (rendering-server+skeleton-bone-get-transform-2d :class 'rendering-server
  :bind "skeleton_bone_get_transform_2d" :hash 1324854622)
 transform-2d (skeleton rid) (bone int))

(defgmethod
 (rendering-server+skeleton-set-base-transform-2d :class 'rendering-server
  :bind "skeleton_set_base_transform_2d" :hash 1246044741)
 :void (skeleton rid) (base-transform transform-2d))

(defgmethod
 (rendering-server+directional-light-create :class 'rendering-server :bind
  "directional_light_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+omni-light-create :class 'rendering-server :bind
  "omni_light_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+spot-light-create :class 'rendering-server :bind
  "spot_light_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+area-light-create :class 'rendering-server :bind
  "area_light_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+light-set-color :class 'rendering-server :bind
  "light_set_color" :hash 2948539648)
 :void (light rid) (color color))

(defgmethod
 (rendering-server+light-set-param :class 'rendering-server :bind
  "light_set_param" :hash 501936875)
 :void (light rid) (param rendering-server+light-param) (value float))

(defgmethod
 (rendering-server+light-set-shadow :class 'rendering-server :bind
  "light_set_shadow" :hash 1265174801)
 :void (light rid) (enabled bool))

(defgmethod
 (rendering-server+light-set-projector :class 'rendering-server :bind
  "light_set_projector" :hash 395945892)
 :void (light rid) (texture rid))

(defgmethod
 (rendering-server+light-set-negative :class 'rendering-server :bind
  "light_set_negative" :hash 1265174801)
 :void (light rid) (enable bool))

(defgmethod
 (rendering-server+light-set-cull-mask :class 'rendering-server :bind
  "light_set_cull_mask" :hash 3411492887)
 :void (light rid) (mask int))

(defgmethod
 (rendering-server+light-set-distance-fade :class 'rendering-server :bind
  "light_set_distance_fade" :hash 1622292572)
 :void (decal rid) (enabled bool) (begin float) (shadow float) (length float))

(defgmethod
 (rendering-server+light-set-reverse-cull-face-mode :class 'rendering-server
  :bind "light_set_reverse_cull_face_mode" :hash 1265174801)
 :void (light rid) (enabled bool))

(defgmethod
 (rendering-server+light-set-shadow-caster-mask :class 'rendering-server :bind
  "light_set_shadow_caster_mask" :hash 3411492887)
 :void (light rid) (mask int))

(defgmethod
 (rendering-server+light-set-bake-mode :class 'rendering-server :bind
  "light_set_bake_mode" :hash 1048525260)
 :void (light rid) (bake-mode rendering-server+light-bake-mode))

(defgmethod
 (rendering-server+light-set-max-sdfgi-cascade :class 'rendering-server :bind
  "light_set_max_sdfgi_cascade" :hash 3411492887)
 :void (light rid) (cascade int))

(defgmethod
 (rendering-server+light-omni-set-shadow-mode :class 'rendering-server :bind
  "light_omni_set_shadow_mode" :hash 2552677200)
 :void (light rid) (mode rendering-server+light-omni-shadow-mode))

(defgmethod
 (rendering-server+light-directional-set-shadow-mode :class 'rendering-server
  :bind "light_directional_set_shadow_mode" :hash 380462970)
 :void (light rid) (mode rendering-server+light-directional-shadow-mode))

(defgmethod
 (rendering-server+light-directional-set-blend-splits :class 'rendering-server
  :bind "light_directional_set_blend_splits" :hash 1265174801)
 :void (light rid) (enable bool))

(defgmethod
 (rendering-server+light-directional-set-sky-mode :class 'rendering-server
  :bind "light_directional_set_sky_mode" :hash 2559740754)
 :void (light rid) (mode rendering-server+light-directional-sky-mode))

(defgmethod
 (rendering-server+light-area-set-size :class 'rendering-server :bind
  "light_area_set_size" :hash 3201125042)
 :void (light rid) (size vector-2))

(defgmethod
 (rendering-server+light-area-set-normalize-energy :class 'rendering-server
  :bind "light_area_set_normalize_energy" :hash 1265174801)
 :void (light rid) (enable bool))

(defgmethod
 (rendering-server+light-projectors-set-filter :class 'rendering-server :bind
  "light_projectors_set_filter" :hash 43944325)
 :void (filter rendering-server+light-projector-filter))

(defgmethod
 (rendering-server+lightmaps-set-bicubic-filter :class 'rendering-server :bind
  "lightmaps_set_bicubic_filter" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (rendering-server+positional-soft-shadow-filter-set-quality :class
  'rendering-server :bind "positional_soft_shadow_filter_set_quality" :hash
  3613045266)
 :void (quality rendering-server+shadow-quality))

(defgmethod
 (rendering-server+directional-soft-shadow-filter-set-quality :class
  'rendering-server :bind "directional_soft_shadow_filter_set_quality" :hash
  3613045266)
 :void (quality rendering-server+shadow-quality))

(defgmethod
 (rendering-server+directional-shadow-atlas-set-size :class 'rendering-server
  :bind "directional_shadow_atlas_set_size" :hash 300928843)
 :void (size int) (is-16bits bool))

(defgmethod
 (rendering-server+reflection-probe-create :class 'rendering-server :bind
  "reflection_probe_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+reflection-probe-set-update-mode :class 'rendering-server
  :bind "reflection_probe_set_update_mode" :hash 3853670147)
 :void (probe rid) (mode rendering-server+reflection-probe-update-mode))

(defgmethod
 (rendering-server+reflection-probe-set-intensity :class 'rendering-server
  :bind "reflection_probe_set_intensity" :hash 1794382983)
 :void (probe rid) (intensity float))

(defgmethod
 (rendering-server+reflection-probe-set-blend-distance :class 'rendering-server
  :bind "reflection_probe_set_blend_distance" :hash 1794382983)
 :void (probe rid) (blend-distance float))

(defgmethod
 (rendering-server+reflection-probe-set-ambient-mode :class 'rendering-server
  :bind "reflection_probe_set_ambient_mode" :hash 184163074)
 :void (probe rid) (mode rendering-server+reflection-probe-ambient-mode))

(defgmethod
 (rendering-server+reflection-probe-set-ambient-color :class 'rendering-server
  :bind "reflection_probe_set_ambient_color" :hash 2948539648)
 :void (probe rid) (color color))

(defgmethod
 (rendering-server+reflection-probe-set-ambient-energy :class 'rendering-server
  :bind "reflection_probe_set_ambient_energy" :hash 1794382983)
 :void (probe rid) (energy float))

(defgmethod
 (rendering-server+reflection-probe-set-max-distance :class 'rendering-server
  :bind "reflection_probe_set_max_distance" :hash 1794382983)
 :void (probe rid) (distance float))

(defgmethod
 (rendering-server+reflection-probe-set-size :class 'rendering-server :bind
  "reflection_probe_set_size" :hash 3227306858)
 :void (probe rid) (size vector-3))

(defgmethod
 (rendering-server+reflection-probe-set-origin-offset :class 'rendering-server
  :bind "reflection_probe_set_origin_offset" :hash 3227306858)
 :void (probe rid) (offset vector-3))

(defgmethod
 (rendering-server+reflection-probe-set-as-interior :class 'rendering-server
  :bind "reflection_probe_set_as_interior" :hash 1265174801)
 :void (probe rid) (enable bool))

(defgmethod
 (rendering-server+reflection-probe-set-enable-box-projection :class
  'rendering-server :bind "reflection_probe_set_enable_box_projection" :hash
  1265174801)
 :void (probe rid) (enable bool))

(defgmethod
 (rendering-server+reflection-probe-set-enable-shadows :class 'rendering-server
  :bind "reflection_probe_set_enable_shadows" :hash 1265174801)
 :void (probe rid) (enable bool))

(defgmethod
 (rendering-server+reflection-probe-set-cull-mask :class 'rendering-server
  :bind "reflection_probe_set_cull_mask" :hash 3411492887)
 :void (probe rid) (layers int))

(defgmethod
 (rendering-server+reflection-probe-set-reflection-mask :class
  'rendering-server :bind "reflection_probe_set_reflection_mask" :hash
  3411492887)
 :void (probe rid) (layers int))

(defgmethod
 (rendering-server+reflection-probe-set-resolution :class 'rendering-server
  :bind "reflection_probe_set_resolution" :hash 3411492887)
 :void (probe rid) (resolution int))

(defgmethod
 (rendering-server+reflection-probe-set-mesh-lod-threshold :class
  'rendering-server :bind "reflection_probe_set_mesh_lod_threshold" :hash
  1794382983)
 :void (probe rid) (pixels float))

(defgmethod
 (rendering-server+decal-create :class 'rendering-server :bind "decal_create"
  :hash 529393457)
 rid)

(defgmethod
 (rendering-server+decal-set-size :class 'rendering-server :bind
  "decal_set_size" :hash 3227306858)
 :void (decal rid) (size vector-3))

(defgmethod
 (rendering-server+decal-set-texture :class 'rendering-server :bind
  "decal_set_texture" :hash 3953344054)
 :void (decal rid) (type rendering-server+decal-texture) (texture rid))

(defgmethod
 (rendering-server+decal-set-emission-energy :class 'rendering-server :bind
  "decal_set_emission_energy" :hash 1794382983)
 :void (decal rid) (energy float))

(defgmethod
 (rendering-server+decal-set-albedo-mix :class 'rendering-server :bind
  "decal_set_albedo_mix" :hash 1794382983)
 :void (decal rid) (albedo-mix float))

(defgmethod
 (rendering-server+decal-set-modulate :class 'rendering-server :bind
  "decal_set_modulate" :hash 2948539648)
 :void (decal rid) (color color))

(defgmethod
 (rendering-server+decal-set-cull-mask :class 'rendering-server :bind
  "decal_set_cull_mask" :hash 3411492887)
 :void (decal rid) (mask int))

(defgmethod
 (rendering-server+decal-set-distance-fade :class 'rendering-server :bind
  "decal_set_distance_fade" :hash 2972769666)
 :void (decal rid) (enabled bool) (begin float) (length float))

(defgmethod
 (rendering-server+decal-set-fade :class 'rendering-server :bind
  "decal_set_fade" :hash 2513314492)
 :void (decal rid) (above float) (below float))

(defgmethod
 (rendering-server+decal-set-normal-fade :class 'rendering-server :bind
  "decal_set_normal_fade" :hash 1794382983)
 :void (decal rid) (fade float))

(defgmethod
 (rendering-server+decals-set-filter :class 'rendering-server :bind
  "decals_set_filter" :hash 3519875702)
 :void (filter rendering-server+decal-filter))

(defgmethod
 (rendering-server+gi-set-use-half-resolution :class 'rendering-server :bind
  "gi_set_use_half_resolution" :hash 2586408642)
 :void (half-resolution bool))

(defgmethod
 (rendering-server+voxel-gi-create :class 'rendering-server :bind
  "voxel_gi_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+voxel-gi-allocate-data :class 'rendering-server :bind
  "voxel_gi_allocate_data" :hash 4108223027)
 :void (voxel-gi rid) (to-cell-xform transform-3d) (aabb aabb)
 (octree-size vector-3i) (octree-cells packed-byte-array)
 (data-cells packed-byte-array) (distance-field packed-byte-array)
 (level-counts packed-int-32array))

(defgmethod
 (rendering-server+voxel-gi-get-octree-size :class 'rendering-server :bind
  "voxel_gi_get_octree_size" :hash 2607699645)
 vector-3i (voxel-gi rid))

(defgmethod
 (rendering-server+voxel-gi-get-octree-cells :class 'rendering-server :bind
  "voxel_gi_get_octree_cells" :hash 3348040486)
 packed-byte-array (voxel-gi rid))

(defgmethod
 (rendering-server+voxel-gi-get-data-cells :class 'rendering-server :bind
  "voxel_gi_get_data_cells" :hash 3348040486)
 packed-byte-array (voxel-gi rid))

(defgmethod
 (rendering-server+voxel-gi-get-distance-field :class 'rendering-server :bind
  "voxel_gi_get_distance_field" :hash 3348040486)
 packed-byte-array (voxel-gi rid))

(defgmethod
 (rendering-server+voxel-gi-get-level-counts :class 'rendering-server :bind
  "voxel_gi_get_level_counts" :hash 788230395)
 packed-int-32array (voxel-gi rid))

(defgmethod
 (rendering-server+voxel-gi-get-to-cell-xform :class 'rendering-server :bind
  "voxel_gi_get_to_cell_xform" :hash 1128465797)
 transform-3d (voxel-gi rid))

(defgmethod
 (rendering-server+voxel-gi-set-dynamic-range :class 'rendering-server :bind
  "voxel_gi_set_dynamic_range" :hash 1794382983)
 :void (voxel-gi rid) (range float))

(defgmethod
 (rendering-server+voxel-gi-set-propagation :class 'rendering-server :bind
  "voxel_gi_set_propagation" :hash 1794382983)
 :void (voxel-gi rid) (amount float))

(defgmethod
 (rendering-server+voxel-gi-set-energy :class 'rendering-server :bind
  "voxel_gi_set_energy" :hash 1794382983)
 :void (voxel-gi rid) (energy float))

(defgmethod
 (rendering-server+voxel-gi-set-baked-exposure-normalization :class
  'rendering-server :bind "voxel_gi_set_baked_exposure_normalization" :hash
  1794382983)
 :void (voxel-gi rid) (baked-exposure float))

(defgmethod
 (rendering-server+voxel-gi-set-bias :class 'rendering-server :bind
  "voxel_gi_set_bias" :hash 1794382983)
 :void (voxel-gi rid) (bias float))

(defgmethod
 (rendering-server+voxel-gi-set-normal-bias :class 'rendering-server :bind
  "voxel_gi_set_normal_bias" :hash 1794382983)
 :void (voxel-gi rid) (bias float))

(defgmethod
 (rendering-server+voxel-gi-set-interior :class 'rendering-server :bind
  "voxel_gi_set_interior" :hash 1265174801)
 :void (voxel-gi rid) (enable bool))

(defgmethod
 (rendering-server+voxel-gi-set-use-two-bounces :class 'rendering-server :bind
  "voxel_gi_set_use_two_bounces" :hash 1265174801)
 :void (voxel-gi rid) (enable bool))

(defgmethod
 (rendering-server+voxel-gi-set-quality :class 'rendering-server :bind
  "voxel_gi_set_quality" :hash 1538689978)
 :void (quality rendering-server+voxel-giquality))

(defgmethod
 (rendering-server+lightmap-create :class 'rendering-server :bind
  "lightmap_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+lightmap-set-textures :class 'rendering-server :bind
  "lightmap_set_textures" :hash 2646464759)
 :void (lightmap rid) (light rid) (uses-sh bool))

(defgmethod
 (rendering-server+lightmap-set-probe-bounds :class 'rendering-server :bind
  "lightmap_set_probe_bounds" :hash 3696536120)
 :void (lightmap rid) (bounds aabb))

(defgmethod
 (rendering-server+lightmap-set-probe-interior :class 'rendering-server :bind
  "lightmap_set_probe_interior" :hash 1265174801)
 :void (lightmap rid) (interior bool))

(defgmethod
 (rendering-server+lightmap-set-probe-capture-data :class 'rendering-server
  :bind "lightmap_set_probe_capture_data" :hash 3217845880)
 :void (lightmap rid) (points packed-vector-3array)
 (point-sh packed-color-array) (tetrahedra packed-int-32array)
 (bsp-tree packed-int-32array))

(defgmethod
 (rendering-server+lightmap-get-probe-capture-points :class 'rendering-server
  :bind "lightmap_get_probe_capture_points" :hash 808965560)
 packed-vector-3array (lightmap rid))

(defgmethod
 (rendering-server+lightmap-get-probe-capture-sh :class 'rendering-server :bind
  "lightmap_get_probe_capture_sh" :hash 1569415609)
 packed-color-array (lightmap rid))

(defgmethod
 (rendering-server+lightmap-get-probe-capture-tetrahedra :class
  'rendering-server :bind "lightmap_get_probe_capture_tetrahedra" :hash
  788230395)
 packed-int-32array (lightmap rid))

(defgmethod
 (rendering-server+lightmap-get-probe-capture-bsp-tree :class 'rendering-server
  :bind "lightmap_get_probe_capture_bsp_tree" :hash 788230395)
 packed-int-32array (lightmap rid))

(defgmethod
 (rendering-server+lightmap-set-baked-exposure-normalization :class
  'rendering-server :bind "lightmap_set_baked_exposure_normalization" :hash
  1794382983)
 :void (lightmap rid) (baked-exposure float))

(defgmethod
 (rendering-server+lightmap-set-probe-capture-update-speed :class
  'rendering-server :bind "lightmap_set_probe_capture_update_speed" :hash
  373806689)
 :void (speed float))

(defgmethod
 (rendering-server+particles-create :class 'rendering-server :bind
  "particles_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+particles-set-mode :class 'rendering-server :bind
  "particles_set_mode" :hash 3492270028)
 :void (particles rid) (mode rendering-server+particles-mode))

(defgmethod
 (rendering-server+particles-set-emitting :class 'rendering-server :bind
  "particles_set_emitting" :hash 1265174801)
 :void (particles rid) (emitting bool))

(defgmethod
 (rendering-server+particles-get-emitting :class 'rendering-server :bind
  "particles_get_emitting" :hash 3521089500)
 bool (particles rid))

(defgmethod
 (rendering-server+particles-set-amount :class 'rendering-server :bind
  "particles_set_amount" :hash 3411492887)
 :void (particles rid) (amount int))

(defgmethod
 (rendering-server+particles-set-amount-ratio :class 'rendering-server :bind
  "particles_set_amount_ratio" :hash 1794382983)
 :void (particles rid) (ratio float))

(defgmethod
 (rendering-server+particles-set-lifetime :class 'rendering-server :bind
  "particles_set_lifetime" :hash 1794382983)
 :void (particles rid) (lifetime float))

(defgmethod
 (rendering-server+particles-set-one-shot :class 'rendering-server :bind
  "particles_set_one_shot" :hash 1265174801)
 :void (particles rid) (one-shot bool))

(defgmethod
 (rendering-server+particles-set-pre-process-time :class 'rendering-server
  :bind "particles_set_pre_process_time" :hash 1794382983)
 :void (particles rid) (time float))

(defgmethod
 (rendering-server+particles-request-process-time :class 'rendering-server
  :bind "particles_request_process_time" :hash 1515254041)
 :void (particles rid) (process-time float) (process-time-residual float))

(defgmethod
 (rendering-server+particles-set-explosiveness-ratio :class 'rendering-server
  :bind "particles_set_explosiveness_ratio" :hash 1794382983)
 :void (particles rid) (ratio float))

(defgmethod
 (rendering-server+particles-set-randomness-ratio :class 'rendering-server
  :bind "particles_set_randomness_ratio" :hash 1794382983)
 :void (particles rid) (ratio float))

(defgmethod
 (rendering-server+particles-set-interp-to-end :class 'rendering-server :bind
  "particles_set_interp_to_end" :hash 1794382983)
 :void (particles rid) (factor float))

(defgmethod
 (rendering-server+particles-set-emitter-velocity :class 'rendering-server
  :bind "particles_set_emitter_velocity" :hash 3227306858)
 :void (particles rid) (velocity vector-3))

(defgmethod
 (rendering-server+particles-set-custom-aabb :class 'rendering-server :bind
  "particles_set_custom_aabb" :hash 3696536120)
 :void (particles rid) (aabb aabb))

(defgmethod
 (rendering-server+particles-set-speed-scale :class 'rendering-server :bind
  "particles_set_speed_scale" :hash 1794382983)
 :void (particles rid) (scale float))

(defgmethod
 (rendering-server+particles-set-use-local-coordinates :class 'rendering-server
  :bind "particles_set_use_local_coordinates" :hash 1265174801)
 :void (particles rid) (enable bool))

(defgmethod
 (rendering-server+particles-set-process-material :class 'rendering-server
  :bind "particles_set_process_material" :hash 395945892)
 :void (particles rid) (material rid))

(defgmethod
 (rendering-server+particles-set-fixed-fps :class 'rendering-server :bind
  "particles_set_fixed_fps" :hash 3411492887)
 :void (particles rid) (fps int))

(defgmethod
 (rendering-server+particles-set-interpolate :class 'rendering-server :bind
  "particles_set_interpolate" :hash 1265174801)
 :void (particles rid) (enable bool))

(defgmethod
 (rendering-server+particles-set-fractional-delta :class 'rendering-server
  :bind "particles_set_fractional_delta" :hash 1265174801)
 :void (particles rid) (enable bool))

(defgmethod
 (rendering-server+particles-set-collision-base-size :class 'rendering-server
  :bind "particles_set_collision_base_size" :hash 1794382983)
 :void (particles rid) (size float))

(defgmethod
 (rendering-server+particles-set-transform-align :class 'rendering-server :bind
  "particles_set_transform_align" :hash 3264971368)
 :void (particles rid) (align rendering-server+particles-transform-align))

(defgmethod
 (rendering-server+particles-set-transform-align-channel-filter :class
  'rendering-server :bind "particles_set_transform_align_channel_filter" :hash
  1303285813)
 :void (particles rid)
 (channel-filter rendering-server+particles-transform-align-custom-src))

(defgmethod
 (rendering-server+particles-set-transform-align-axis :class 'rendering-server
  :bind "particles_set_transform_align_axis" :hash 3065310065)
 :void (particles rid)
 (rotation-axis rendering-server+particles-transform-align-axis))

(defgmethod
 (rendering-server+particles-set-trails :class 'rendering-server :bind
  "particles_set_trails" :hash 2010054925)
 :void (particles rid) (enable bool) (length-sec float))

(defgmethod
 (rendering-server+particles-set-trail-bind-poses :class 'rendering-server
  :bind "particles_set_trail_bind_poses" :hash 684822712)
 :void (particles rid) (bind-poses array))

(defgmethod
 (rendering-server+particles-is-inactive :class 'rendering-server :bind
  "particles_is_inactive" :hash 3521089500)
 bool (particles rid))

(defgmethod
 (rendering-server+particles-request-process :class 'rendering-server :bind
  "particles_request_process" :hash 2722037293)
 :void (particles rid))

(defgmethod
 (rendering-server+particles-restart :class 'rendering-server :bind
  "particles_restart" :hash 2722037293)
 :void (particles rid))

(defgmethod
 (rendering-server+particles-set-subemitter :class 'rendering-server :bind
  "particles_set_subemitter" :hash 395945892)
 :void (particles rid) (subemitter-particles rid))

(defgmethod
 (rendering-server+particles-emit :class 'rendering-server :bind
  "particles_emit" :hash 4043136117)
 :void (particles rid) (transform transform-3d) (velocity vector-3)
 (color color) (custom color) (emit-flags int))

(defgmethod
 (rendering-server+particles-set-draw-order :class 'rendering-server :bind
  "particles_set_draw_order" :hash 935028487)
 :void (particles rid) (order rendering-server+particles-draw-order))

(defgmethod
 (rendering-server+particles-set-draw-passes :class 'rendering-server :bind
  "particles_set_draw_passes" :hash 3411492887)
 :void (particles rid) (count int))

(defgmethod
 (rendering-server+particles-set-draw-pass-mesh :class 'rendering-server :bind
  "particles_set_draw_pass_mesh" :hash 2310537182)
 :void (particles rid) (pass int) (mesh rid))

(defgmethod
 (rendering-server+particles-get-current-aabb :class 'rendering-server :bind
  "particles_get_current_aabb" :hash 3952830260)
 aabb (particles rid))

(defgmethod
 (rendering-server+particles-set-emission-transform :class 'rendering-server
  :bind "particles_set_emission_transform" :hash 3935195649)
 :void (particles rid) (transform transform-3d))

(defgmethod
 (rendering-server+particles-collision-create :class 'rendering-server :bind
  "particles_collision_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+particles-collision-set-collision-type :class
  'rendering-server :bind "particles_collision_set_collision_type" :hash
  1497044930)
 :void (particles-collision rid)
 (type rendering-server+particles-collision-type))

(defgmethod
 (rendering-server+particles-collision-set-cull-mask :class 'rendering-server
  :bind "particles_collision_set_cull_mask" :hash 3411492887)
 :void (particles-collision rid) (mask int))

(defgmethod
 (rendering-server+particles-collision-set-sphere-radius :class
  'rendering-server :bind "particles_collision_set_sphere_radius" :hash
  1794382983)
 :void (particles-collision rid) (radius float))

(defgmethod
 (rendering-server+particles-collision-set-box-extents :class 'rendering-server
  :bind "particles_collision_set_box_extents" :hash 3227306858)
 :void (particles-collision rid) (extents vector-3))

(defgmethod
 (rendering-server+particles-collision-set-attractor-strength :class
  'rendering-server :bind "particles_collision_set_attractor_strength" :hash
  1794382983)
 :void (particles-collision rid) (strength float))

(defgmethod
 (rendering-server+particles-collision-set-attractor-directionality :class
  'rendering-server :bind "particles_collision_set_attractor_directionality"
  :hash 1794382983)
 :void (particles-collision rid) (amount float))

(defgmethod
 (rendering-server+particles-collision-set-attractor-attenuation :class
  'rendering-server :bind "particles_collision_set_attractor_attenuation" :hash
  1794382983)
 :void (particles-collision rid) (curve float))

(defgmethod
 (rendering-server+particles-collision-set-field-texture :class
  'rendering-server :bind "particles_collision_set_field_texture" :hash
  395945892)
 :void (particles-collision rid) (texture rid))

(defgmethod
 (rendering-server+particles-collision-height-field-update :class
  'rendering-server :bind "particles_collision_height_field_update" :hash
  2722037293)
 :void (particles-collision rid))

(defgmethod
 (rendering-server+particles-collision-set-height-field-resolution :class
  'rendering-server :bind "particles_collision_set_height_field_resolution"
  :hash 962977297)
 :void (particles-collision rid)
 (resolution rendering-server+particles-collision-heightfield-resolution))

(defgmethod
 (rendering-server+particles-collision-set-height-field-mask :class
  'rendering-server :bind "particles_collision_set_height_field_mask" :hash
  3411492887)
 :void (particles-collision rid) (mask int))

(defgmethod
 (rendering-server+fog-volume-create :class 'rendering-server :bind
  "fog_volume_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+fog-volume-set-shape :class 'rendering-server :bind
  "fog_volume_set_shape" :hash 3818703106)
 :void (fog-volume rid) (shape rendering-server+fog-volume-shape))

(defgmethod
 (rendering-server+fog-volume-set-size :class 'rendering-server :bind
  "fog_volume_set_size" :hash 3227306858)
 :void (fog-volume rid) (size vector-3))

(defgmethod
 (rendering-server+fog-volume-set-material :class 'rendering-server :bind
  "fog_volume_set_material" :hash 395945892)
 :void (fog-volume rid) (material rid))

(defgmethod
 (rendering-server+visibility-notifier-create :class 'rendering-server :bind
  "visibility_notifier_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+visibility-notifier-set-aabb :class 'rendering-server :bind
  "visibility_notifier_set_aabb" :hash 3696536120)
 :void (notifier rid) (aabb aabb))

(defgmethod
 (rendering-server+visibility-notifier-set-callbacks :class 'rendering-server
  :bind "visibility_notifier_set_callbacks" :hash 2689735388)
 :void (notifier rid) (enter-callable callable) (exit-callable callable))

(defgmethod
 (rendering-server+occluder-create :class 'rendering-server :bind
  "occluder_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+occluder-set-mesh :class 'rendering-server :bind
  "occluder_set_mesh" :hash 3854404263)
 :void (occluder rid) (vertices packed-vector-3array)
 (indices packed-int-32array))

(defgmethod
 (rendering-server+camera-create :class 'rendering-server :bind "camera_create"
  :hash 529393457)
 rid)

(defgmethod
 (rendering-server+camera-set-perspective :class 'rendering-server :bind
  "camera_set_perspective" :hash 157498339)
 :void (camera rid) (fovy-degrees float) (z-near float) (z-far float))

(defgmethod
 (rendering-server+camera-set-orthogonal :class 'rendering-server :bind
  "camera_set_orthogonal" :hash 157498339)
 :void (camera rid) (size float) (z-near float) (z-far float))

(defgmethod
 (rendering-server+camera-set-frustum :class 'rendering-server :bind
  "camera_set_frustum" :hash 1889878953)
 :void (camera rid) (size float) (offset vector-2) (z-near float) (z-far float))

(defgmethod
 (rendering-server+camera-set-transform :class 'rendering-server :bind
  "camera_set_transform" :hash 3935195649)
 :void (camera rid) (transform transform-3d))

(defgmethod
 (rendering-server+camera-set-cull-mask :class 'rendering-server :bind
  "camera_set_cull_mask" :hash 3411492887)
 :void (camera rid) (layers int))

(defgmethod
 (rendering-server+camera-set-environment :class 'rendering-server :bind
  "camera_set_environment" :hash 395945892)
 :void (camera rid) (env rid))

(defgmethod
 (rendering-server+camera-set-camera-attributes :class 'rendering-server :bind
  "camera_set_camera_attributes" :hash 395945892)
 :void (camera rid) (effects rid))

(defgmethod
 (rendering-server+camera-set-compositor :class 'rendering-server :bind
  "camera_set_compositor" :hash 395945892)
 :void (camera rid) (compositor rid))

(defgmethod
 (rendering-server+camera-set-use-vertical-aspect :class 'rendering-server
  :bind "camera_set_use_vertical_aspect" :hash 1265174801)
 :void (camera rid) (enable bool))

(defgmethod
 (rendering-server+viewport-create :class 'rendering-server :bind
  "viewport_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+viewport-set-use-xr :class 'rendering-server :bind
  "viewport_set_use_xr" :hash 1265174801)
 :void (viewport rid) (use-xr bool))

(defgmethod
 (rendering-server+viewport-set-size :class 'rendering-server :bind
  "viewport_set_size" :hash 3313592705)
 :void (viewport rid) (width int) (height int) (view-count int))

(defgmethod
 (rendering-server+viewport-set-active :class 'rendering-server :bind
  "viewport_set_active" :hash 1265174801)
 :void (viewport rid) (active bool))

(defgmethod
 (rendering-server+viewport-set-parent-viewport :class 'rendering-server :bind
  "viewport_set_parent_viewport" :hash 395945892)
 :void (viewport rid) (parent-viewport rid))

(defgmethod
 (rendering-server+viewport-attach-to-screen :class 'rendering-server :bind
  "viewport_attach_to_screen" :hash 1062245816)
 :void (viewport rid) (rect rect-2) (screen int))

(defgmethod
 (rendering-server+viewport-set-render-direct-to-screen :class
  'rendering-server :bind "viewport_set_render_direct_to_screen" :hash
  1265174801)
 :void (viewport rid) (enabled bool))

(defgmethod
 (rendering-server+viewport-set-canvas-cull-mask :class 'rendering-server :bind
  "viewport_set_canvas_cull_mask" :hash 3411492887)
 :void (viewport rid) (canvas-cull-mask int))

(defgmethod
 (rendering-server+viewport-set-scaling-3d-mode :class 'rendering-server :bind
  "viewport_set_scaling_3d_mode" :hash 2386524376)
 :void (viewport rid)
 (scaling-3d-mode rendering-server+viewport-scaling-3dmode))

(defgmethod
 (rendering-server+viewport-set-scaling-3d-scale :class 'rendering-server :bind
  "viewport_set_scaling_3d_scale" :hash 1794382983)
 :void (viewport rid) (scale float))

(defgmethod
 (rendering-server+viewport-set-fsr-sharpness :class 'rendering-server :bind
  "viewport_set_fsr_sharpness" :hash 1794382983)
 :void (viewport rid) (sharpness float))

(defgmethod
 (rendering-server+viewport-set-texture-mipmap-bias :class 'rendering-server
  :bind "viewport_set_texture_mipmap_bias" :hash 1794382983)
 :void (viewport rid) (mipmap-bias float))

(defgmethod
 (rendering-server+viewport-set-anisotropic-filtering-level :class
  'rendering-server :bind "viewport_set_anisotropic_filtering_level" :hash
  3953214029)
 :void (viewport rid)
 (anisotropic-filtering-level rendering-server+viewport-anisotropic-filtering))

(defgmethod
 (rendering-server+viewport-set-update-mode :class 'rendering-server :bind
  "viewport_set_update_mode" :hash 3161116010)
 :void (viewport rid) (update-mode rendering-server+viewport-update-mode))

(defgmethod
 (rendering-server+viewport-get-update-mode :class 'rendering-server :bind
  "viewport_get_update_mode" :hash 3803901472)
 rendering-server+viewport-update-mode (viewport rid))

(defgmethod
 (rendering-server+viewport-set-clear-mode :class 'rendering-server :bind
  "viewport_set_clear_mode" :hash 3628367896)
 :void (viewport rid) (clear-mode rendering-server+viewport-clear-mode))

(defgmethod
 (rendering-server+viewport-get-render-target :class 'rendering-server :bind
  "viewport_get_render_target" :hash 3814569979)
 rid (viewport rid))

(defgmethod
 (rendering-server+viewport-get-texture :class 'rendering-server :bind
  "viewport_get_texture" :hash 3814569979)
 rid (viewport rid))

(defgmethod
 (rendering-server+viewport-set-disable-3d :class 'rendering-server :bind
  "viewport_set_disable_3d" :hash 1265174801)
 :void (viewport rid) (disable bool))

(defgmethod
 (rendering-server+viewport-set-disable-2d :class 'rendering-server :bind
  "viewport_set_disable_2d" :hash 1265174801)
 :void (viewport rid) (disable bool))

(defgmethod
 (rendering-server+viewport-set-environment-mode :class 'rendering-server :bind
  "viewport_set_environment_mode" :hash 2196892182)
 :void (viewport rid) (mode rendering-server+viewport-environment-mode))

(defgmethod
 (rendering-server+viewport-attach-camera :class 'rendering-server :bind
  "viewport_attach_camera" :hash 395945892)
 :void (viewport rid) (camera rid))

(defgmethod
 (rendering-server+viewport-set-scenario :class 'rendering-server :bind
  "viewport_set_scenario" :hash 395945892)
 :void (viewport rid) (scenario rid))

(defgmethod
 (rendering-server+viewport-attach-canvas :class 'rendering-server :bind
  "viewport_attach_canvas" :hash 395945892)
 :void (viewport rid) (canvas rid))

(defgmethod
 (rendering-server+viewport-remove-canvas :class 'rendering-server :bind
  "viewport_remove_canvas" :hash 395945892)
 :void (viewport rid) (canvas rid))

(defgmethod
 (rendering-server+viewport-set-snap-2d-transforms-to-pixel :class
  'rendering-server :bind "viewport_set_snap_2d_transforms_to_pixel" :hash
  1265174801)
 :void (viewport rid) (enabled bool))

(defgmethod
 (rendering-server+viewport-set-snap-2d-vertices-to-pixel :class
  'rendering-server :bind "viewport_set_snap_2d_vertices_to_pixel" :hash
  1265174801)
 :void (viewport rid) (enabled bool))

(defgmethod
 (rendering-server+viewport-set-default-canvas-item-texture-filter :class
  'rendering-server :bind "viewport_set_default_canvas_item_texture_filter"
  :hash 1155129294)
 :void (viewport rid) (filter rendering-server+canvas-item-texture-filter))

(defgmethod
 (rendering-server+viewport-set-default-canvas-item-texture-repeat :class
  'rendering-server :bind "viewport_set_default_canvas_item_texture_repeat"
  :hash 1652956681)
 :void (viewport rid) (repeat rendering-server+canvas-item-texture-repeat))

(defgmethod
 (rendering-server+viewport-set-canvas-transform :class 'rendering-server :bind
  "viewport_set_canvas_transform" :hash 3608606053)
 :void (viewport rid) (canvas rid) (offset transform-2d))

(defgmethod
 (rendering-server+viewport-set-canvas-stacking :class 'rendering-server :bind
  "viewport_set_canvas_stacking" :hash 3713930247)
 :void (viewport rid) (canvas rid) (layer int) (sublayer int))

(defgmethod
 (rendering-server+viewport-set-transparent-background :class 'rendering-server
  :bind "viewport_set_transparent_background" :hash 1265174801)
 :void (viewport rid) (enabled bool))

(defgmethod
 (rendering-server+viewport-set-global-canvas-transform :class
  'rendering-server :bind "viewport_set_global_canvas_transform" :hash
  1246044741)
 :void (viewport rid) (transform transform-2d))

(defgmethod
 (rendering-server+viewport-set-sdf-oversize-and-scale :class 'rendering-server
  :bind "viewport_set_sdf_oversize_and_scale" :hash 1329198632)
 :void (viewport rid) (oversize rendering-server+viewport-sdfoversize)
 (scale rendering-server+viewport-sdfscale))

(defgmethod
 (rendering-server+viewport-set-positional-shadow-atlas-size :class
  'rendering-server :bind "viewport_set_positional_shadow_atlas_size" :hash
  1904426712)
 :void (viewport rid) (size int) (use-16-bits bool))

(defgmethod
 (rendering-server+viewport-set-positional-shadow-atlas-quadrant-subdivision
  :class 'rendering-server :bind
  "viewport_set_positional_shadow_atlas_quadrant_subdivision" :hash 4288446313)
 :void (viewport rid) (quadrant int) (subdivision int))

(defgmethod
 (rendering-server+viewport-set-msaa-3d :class 'rendering-server :bind
  "viewport_set_msaa_3d" :hash 3764433340)
 :void (viewport rid) (msaa rendering-server+viewport-msaa))

(defgmethod
 (rendering-server+viewport-set-msaa-2d :class 'rendering-server :bind
  "viewport_set_msaa_2d" :hash 3764433340)
 :void (viewport rid) (msaa rendering-server+viewport-msaa))

(defgmethod
 (rendering-server+viewport-set-use-hdr-2d :class 'rendering-server :bind
  "viewport_set_use_hdr_2d" :hash 1265174801)
 :void (viewport rid) (enabled bool))

(defgmethod
 (rendering-server+viewport-set-screen-space-aa :class 'rendering-server :bind
  "viewport_set_screen_space_aa" :hash 1447279591)
 :void (viewport rid) (mode rendering-server+viewport-screen-space-aa))

(defgmethod
 (rendering-server+viewport-set-use-taa :class 'rendering-server :bind
  "viewport_set_use_taa" :hash 1265174801)
 :void (viewport rid) (enable bool))

(defgmethod
 (rendering-server+viewport-set-use-debanding :class 'rendering-server :bind
  "viewport_set_use_debanding" :hash 1265174801)
 :void (viewport rid) (enable bool))

(defgmethod
 (rendering-server+viewport-set-use-occlusion-culling :class 'rendering-server
  :bind "viewport_set_use_occlusion_culling" :hash 1265174801)
 :void (viewport rid) (enable bool))

(defgmethod
 (rendering-server+viewport-set-occlusion-rays-per-thread :class
  'rendering-server :bind "viewport_set_occlusion_rays_per_thread" :hash
  1286410249)
 :void (rays-per-thread int))

(defgmethod
 (rendering-server+viewport-set-occlusion-culling-build-quality :class
  'rendering-server :bind "viewport_set_occlusion_culling_build_quality" :hash
  2069725696)
 :void (quality rendering-server+viewport-occlusion-culling-build-quality))

(defgmethod
 (rendering-server+viewport-get-render-info :class 'rendering-server :bind
  "viewport_get_render_info" :hash 2041262392)
 int (viewport rid) (type rendering-server+viewport-render-info-type)
 (info rendering-server+viewport-render-info))

(defgmethod
 (rendering-server+viewport-set-debug-draw :class 'rendering-server :bind
  "viewport_set_debug_draw" :hash 2089420930)
 :void (viewport rid) (draw rendering-server+viewport-debug-draw))

(defgmethod
 (rendering-server+viewport-set-measure-render-time :class 'rendering-server
  :bind "viewport_set_measure_render_time" :hash 1265174801)
 :void (viewport rid) (enable bool))

(defgmethod
 (rendering-server+viewport-get-measured-render-time-cpu :class
  'rendering-server :bind "viewport_get_measured_render_time_cpu" :hash
  866169185)
 float (viewport rid))

(defgmethod
 (rendering-server+viewport-get-measured-render-time-gpu :class
  'rendering-server :bind "viewport_get_measured_render_time_gpu" :hash
  866169185)
 float (viewport rid))

(defgmethod
 (rendering-server+viewport-set-vrs-mode :class 'rendering-server :bind
  "viewport_set_vrs_mode" :hash 398809874)
 :void (viewport rid) (mode rendering-server+viewport-vrsmode))

(defgmethod
 (rendering-server+viewport-set-vrs-update-mode :class 'rendering-server :bind
  "viewport_set_vrs_update_mode" :hash 2696154815)
 :void (viewport rid) (mode rendering-server+viewport-vrsupdate-mode))

(defgmethod
 (rendering-server+viewport-set-vrs-texture :class 'rendering-server :bind
  "viewport_set_vrs_texture" :hash 395945892)
 :void (viewport rid) (texture rid))

(defgmethod
 (rendering-server+sky-create :class 'rendering-server :bind "sky_create" :hash
  529393457)
 rid)

(defgmethod
 (rendering-server+sky-set-radiance-size :class 'rendering-server :bind
  "sky_set_radiance_size" :hash 3411492887)
 :void (sky rid) (radiance-size int))

(defgmethod
 (rendering-server+sky-set-mode :class 'rendering-server :bind "sky_set_mode"
  :hash 3279019937)
 :void (sky rid) (mode rendering-server+sky-mode))

(defgmethod
 (rendering-server+sky-set-material :class 'rendering-server :bind
  "sky_set_material" :hash 395945892)
 :void (sky rid) (material rid))

(defgmethod
 (rendering-server+sky-bake-panorama :class 'rendering-server :bind
  "sky_bake_panorama" :hash 3875285818)
 image (sky rid) (energy float) (bake-irradiance bool) (size vector-2i))

(defgmethod
 (rendering-server+compositor-effect-create :class 'rendering-server :bind
  "compositor_effect_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+compositor-effect-set-enabled :class 'rendering-server :bind
  "compositor_effect_set_enabled" :hash 1265174801)
 :void (effect rid) (enabled bool))

(defgmethod
 (rendering-server+compositor-effect-set-callback :class 'rendering-server
  :bind "compositor_effect_set_callback" :hash 487412485)
 :void (effect rid)
 (callback-type rendering-server+compositor-effect-callback-type)
 (callback callable))

(defgmethod
 (rendering-server+compositor-effect-set-flag :class 'rendering-server :bind
  "compositor_effect_set_flag" :hash 3659527075)
 :void (effect rid) (flag rendering-server+compositor-effect-flags) (set bool))

(defgmethod
 (rendering-server+compositor-create :class 'rendering-server :bind
  "compositor_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+compositor-set-compositor-effects :class 'rendering-server
  :bind "compositor_set_compositor_effects" :hash 684822712)
 :void (compositor rid) (effects array))

(defgmethod
 (rendering-server+environment-create :class 'rendering-server :bind
  "environment_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+environment-set-background :class 'rendering-server :bind
  "environment_set_background" :hash 3937328877)
 :void (env rid) (bg rendering-server+environment-bg))

(defgmethod
 (rendering-server+environment-set-camera-id :class 'rendering-server :bind
  "environment_set_camera_id" :hash 3411492887)
 :void (env rid) (id int))

(defgmethod
 (rendering-server+environment-set-sky :class 'rendering-server :bind
  "environment_set_sky" :hash 395945892)
 :void (env rid) (sky rid))

(defgmethod
 (rendering-server+environment-set-sky-custom-fov :class 'rendering-server
  :bind "environment_set_sky_custom_fov" :hash 1794382983)
 :void (env rid) (scale float))

(defgmethod
 (rendering-server+environment-set-sky-orientation :class 'rendering-server
  :bind "environment_set_sky_orientation" :hash 1735850857)
 :void (env rid) (orientation basis))

(defgmethod
 (rendering-server+environment-set-bg-color :class 'rendering-server :bind
  "environment_set_bg_color" :hash 2948539648)
 :void (env rid) (color color))

(defgmethod
 (rendering-server+environment-set-bg-energy :class 'rendering-server :bind
  "environment_set_bg_energy" :hash 2513314492)
 :void (env rid) (multiplier float) (exposure-value float))

(defgmethod
 (rendering-server+environment-set-canvas-max-layer :class 'rendering-server
  :bind "environment_set_canvas_max_layer" :hash 3411492887)
 :void (env rid) (max-layer int))

(defgmethod
 (rendering-server+environment-set-ambient-light :class 'rendering-server :bind
  "environment_set_ambient_light" :hash 1214961493)
 :void (env rid) (color color)
 (ambient rendering-server+environment-ambient-source) (energy float)
 (sky-contribution float)
 (reflection-source rendering-server+environment-reflection-source))

(defgmethod
 (rendering-server+environment-set-glow :class 'rendering-server :bind
  "environment_set_glow" :hash 2421724940)
 :void (env rid) (enable bool) (levels packed-float-32array) (intensity float)
 (strength float) (mix float) (bloom-threshold float)
 (blend-mode rendering-server+environment-glow-blend-mode)
 (hdr-bleed-threshold float) (hdr-bleed-scale float) (hdr-luminance-cap float)
 (glow-map-strength float) (glow-map rid))

(defgmethod
 (rendering-server+environment-set-tonemap :class 'rendering-server :bind
  "environment_set_tonemap" :hash 2914312638)
 :void (env rid) (tone-mapper rendering-server+environment-tone-mapper)
 (exposure float) (white float))

(defgmethod
 (rendering-server+environment-set-tonemap-agx-contrast :class
  'rendering-server :bind "environment_set_tonemap_agx_contrast" :hash
  1794382983)
 :void (env rid) (agx-contrast float))

(defgmethod
 (rendering-server+environment-set-adjustment :class 'rendering-server :bind
  "environment_set_adjustment" :hash 876799838)
 :void (env rid) (enable bool) (brightness float) (contrast float)
 (saturation float) (use-1d-color-correction bool) (color-correction rid))

(defgmethod
 (rendering-server+environment-set-ssr :class 'rendering-server :bind
  "environment_set_ssr" :hash 3607294374)
 :void (env rid) (enable bool) (max-steps int) (fade-in float) (fade-out float)
 (depth-tolerance float))

(defgmethod
 (rendering-server+environment-set-ssao :class 'rendering-server :bind
  "environment_set_ssao" :hash 3994732740)
 :void (env rid) (enable bool) (radius float) (intensity float) (power float)
 (detail float) (horizon float) (sharpness float) (light-affect float)
 (ao-channel-affect float))

(defgmethod
 (rendering-server+environment-set-fog :class 'rendering-server :bind
  "environment_set_fog" :hash 105051629)
 :void (env rid) (enable bool) (light-color color) (light-energy float)
 (sun-scatter float) (density float) (height float) (height-density float)
 (aerial-perspective float) (sky-affect float)
 (fog-mode rendering-server+environment-fog-mode))

(defgmethod
 (rendering-server+environment-set-fog-depth :class 'rendering-server :bind
  "environment_set_fog_depth" :hash 157498339)
 :void (env rid) (curve float) (begin float) (end float))

(defgmethod
 (rendering-server+environment-set-sdfgi :class 'rendering-server :bind
  "environment_set_sdfgi" :hash 3519144388)
 :void (env rid) (enable bool) (cascades int) (min-cell-size float)
 (y-scale rendering-server+environment-sdfgiyscale) (use-occlusion bool)
 (bounce-feedback float) (read-sky bool) (energy float) (normal-bias float)
 (probe-bias float))

(defgmethod
 (rendering-server+environment-set-volumetric-fog :class 'rendering-server
  :bind "environment_set_volumetric_fog" :hash 1553633833)
 :void (env rid) (enable bool) (density float) (albedo color) (emission color)
 (emission-energy float) (anisotropy float) (length float)
 (detail-spread float) (gi-inject float) (temporal-reprojection bool)
 (temporal-reprojection-amount float) (ambient-inject float) (sky-affect float))

(defgmethod
 (rendering-server+environment-glow-set-use-bicubic-upscale :class
  'rendering-server :bind "environment_glow_set_use_bicubic_upscale" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (rendering-server+environment-set-ssr-half-size :class 'rendering-server :bind
  "environment_set_ssr_half_size" :hash 2586408642)
 :void (half-size bool))

(defgmethod
 (rendering-server+environment-set-ssr-roughness-quality :class
  'rendering-server :bind "environment_set_ssr_roughness_quality" :hash
  1190026788)
 :void (quality rendering-server+environment-ssrroughness-quality))

(defgmethod
 (rendering-server+environment-set-ssao-quality :class 'rendering-server :bind
  "environment_set_ssao_quality" :hash 189753569)
 :void (quality rendering-server+environment-ssaoquality) (half-size bool)
 (adaptive-target float) (blur-passes int) (fadeout-from float)
 (fadeout-to float))

(defgmethod
 (rendering-server+environment-set-ssil-quality :class 'rendering-server :bind
  "environment_set_ssil_quality" :hash 1713836683)
 :void (quality rendering-server+environment-ssilquality) (half-size bool)
 (adaptive-target float) (blur-passes int) (fadeout-from float)
 (fadeout-to float))

(defgmethod
 (rendering-server+environment-set-sdfgi-ray-count :class 'rendering-server
  :bind "environment_set_sdfgi_ray_count" :hash 340137951)
 :void (ray-count rendering-server+environment-sdfgiray-count))

(defgmethod
 (rendering-server+environment-set-sdfgi-frames-to-converge :class
  'rendering-server :bind "environment_set_sdfgi_frames_to_converge" :hash
  2182444374)
 :void (frames rendering-server+environment-sdfgiframes-to-converge))

(defgmethod
 (rendering-server+environment-set-sdfgi-frames-to-update-light :class
  'rendering-server :bind "environment_set_sdfgi_frames_to_update_light" :hash
  1251144068)
 :void (frames rendering-server+environment-sdfgiframes-to-update-light))

(defgmethod
 (rendering-server+environment-set-volumetric-fog-volume-size :class
  'rendering-server :bind "environment_set_volumetric_fog_volume_size" :hash
  3937882851)
 :void (size int) (depth int))

(defgmethod
 (rendering-server+environment-set-volumetric-fog-filter-active :class
  'rendering-server :bind "environment_set_volumetric_fog_filter_active" :hash
  2586408642)
 :void (active bool))

(defgmethod
 (rendering-server+environment-bake-panorama :class 'rendering-server :bind
  "environment_bake_panorama" :hash 2452908646)
 image (environment rid) (bake-irradiance bool) (size vector-2i))

(defgmethod
 (rendering-server+screen-space-roughness-limiter-set-active :class
  'rendering-server :bind "screen_space_roughness_limiter_set_active" :hash
  916716790)
 :void (enable bool) (amount float) (limit float))

(defgmethod
 (rendering-server+sub-surface-scattering-set-quality :class 'rendering-server
  :bind "sub_surface_scattering_set_quality" :hash 64571803)
 :void (quality rendering-server+sub-surface-scattering-quality))

(defgmethod
 (rendering-server+sub-surface-scattering-set-scale :class 'rendering-server
  :bind "sub_surface_scattering_set_scale" :hash 1017552074)
 :void (scale float) (depth-scale float))

(defgmethod
 (rendering-server+camera-attributes-create :class 'rendering-server :bind
  "camera_attributes_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+camera-attributes-set-dof-blur-quality :class
  'rendering-server :bind "camera_attributes_set_dof_blur_quality" :hash
  2220136795)
 :void (quality rendering-server+dofblur-quality) (use-jitter bool))

(defgmethod
 (rendering-server+camera-attributes-set-dof-blur-bokeh-shape :class
  'rendering-server :bind "camera_attributes_set_dof_blur_bokeh_shape" :hash
  1205058394)
 :void (shape rendering-server+dofbokeh-shape))

(defgmethod
 (rendering-server+camera-attributes-set-dof-blur :class 'rendering-server
  :bind "camera_attributes_set_dof_blur" :hash 316272616)
 :void (camera-attributes rid) (far-enable bool) (far-distance float)
 (far-transition float) (near-enable bool) (near-distance float)
 (near-transition float) (amount float))

(defgmethod
 (rendering-server+camera-attributes-set-exposure :class 'rendering-server
  :bind "camera_attributes_set_exposure" :hash 2513314492)
 :void (camera-attributes rid) (multiplier float) (normalization float))

(defgmethod
 (rendering-server+camera-attributes-set-auto-exposure :class 'rendering-server
  :bind "camera_attributes_set_auto_exposure" :hash 4266986332)
 :void (camera-attributes rid) (enable bool) (min-sensitivity float)
 (max-sensitivity float) (speed float) (scale float))

(defgmethod
 (rendering-server+scenario-create :class 'rendering-server :bind
  "scenario_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+scenario-set-environment :class 'rendering-server :bind
  "scenario_set_environment" :hash 395945892)
 :void (scenario rid) (environment rid))

(defgmethod
 (rendering-server+scenario-set-fallback-environment :class 'rendering-server
  :bind "scenario_set_fallback_environment" :hash 395945892)
 :void (scenario rid) (environment rid))

(defgmethod
 (rendering-server+scenario-set-camera-attributes :class 'rendering-server
  :bind "scenario_set_camera_attributes" :hash 395945892)
 :void (scenario rid) (effects rid))

(defgmethod
 (rendering-server+scenario-set-compositor :class 'rendering-server :bind
  "scenario_set_compositor" :hash 395945892)
 :void (scenario rid) (compositor rid))

(defgmethod
 (rendering-server+instance-create2 :class 'rendering-server :bind
  "instance_create2" :hash 746547085)
 rid (base rid) (scenario rid))

(defgmethod
 (rendering-server+instance-create :class 'rendering-server :bind
  "instance_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+instance-set-base :class 'rendering-server :bind
  "instance_set_base" :hash 395945892)
 :void (instance rid) (base rid))

(defgmethod
 (rendering-server+instance-set-scenario :class 'rendering-server :bind
  "instance_set_scenario" :hash 395945892)
 :void (instance rid) (scenario rid))

(defgmethod
 (rendering-server+instance-set-layer-mask :class 'rendering-server :bind
  "instance_set_layer_mask" :hash 3411492887)
 :void (instance rid) (mask int))

(defgmethod
 (rendering-server+instance-set-pivot-data :class 'rendering-server :bind
  "instance_set_pivot_data" :hash 1280615259)
 :void (instance rid) (sorting-offset float) (use-aabb-center bool))

(defgmethod
 (rendering-server+instance-set-transform :class 'rendering-server :bind
  "instance_set_transform" :hash 3935195649)
 :void (instance rid) (transform transform-3d))

(defgmethod
 (rendering-server+instance-attach-object-instance-id :class 'rendering-server
  :bind "instance_attach_object_instance_id" :hash 3411492887)
 :void (instance rid) (id int))

(defgmethod
 (rendering-server+instance-set-blend-shape-weight :class 'rendering-server
  :bind "instance_set_blend_shape_weight" :hash 1892459533)
 :void (instance rid) (shape int) (weight float))

(defgmethod
 (rendering-server+instance-set-surface-override-material :class
  'rendering-server :bind "instance_set_surface_override_material" :hash
  2310537182)
 :void (instance rid) (surface int) (material rid))

(defgmethod
 (rendering-server+instance-set-visible :class 'rendering-server :bind
  "instance_set_visible" :hash 1265174801)
 :void (instance rid) (visible bool))

(defgmethod
 (rendering-server+instance-geometry-set-transparency :class 'rendering-server
  :bind "instance_geometry_set_transparency" :hash 1794382983)
 :void (instance rid) (transparency float))

(defgmethod
 (rendering-server+instance-teleport :class 'rendering-server :bind
  "instance_teleport" :hash 2722037293)
 :void (instance rid))

(defgmethod
 (rendering-server+instance-set-custom-aabb :class 'rendering-server :bind
  "instance_set_custom_aabb" :hash 3696536120)
 :void (instance rid) (aabb aabb))

(defgmethod
 (rendering-server+instance-attach-skeleton :class 'rendering-server :bind
  "instance_attach_skeleton" :hash 395945892)
 :void (instance rid) (skeleton rid))

(defgmethod
 (rendering-server+instance-set-extra-visibility-margin :class
  'rendering-server :bind "instance_set_extra_visibility_margin" :hash
  1794382983)
 :void (instance rid) (margin float))

(defgmethod
 (rendering-server+instance-set-visibility-parent :class 'rendering-server
  :bind "instance_set_visibility_parent" :hash 395945892)
 :void (instance rid) (parent rid))

(defgmethod
 (rendering-server+instance-set-ignore-culling :class 'rendering-server :bind
  "instance_set_ignore_culling" :hash 1265174801)
 :void (instance rid) (enabled bool))

(defgmethod
 (rendering-server+instance-geometry-set-flag :class 'rendering-server :bind
  "instance_geometry_set_flag" :hash 1014989537)
 :void (instance rid) (flag rendering-server+instance-flags) (enabled bool))

(defgmethod
 (rendering-server+instance-geometry-set-cast-shadows-setting :class
  'rendering-server :bind "instance_geometry_set_cast_shadows_setting" :hash
  3768836020)
 :void (instance rid)
 (shadow-casting-setting rendering-server+shadow-casting-setting))

(defgmethod
 (rendering-server+instance-geometry-set-material-override :class
  'rendering-server :bind "instance_geometry_set_material_override" :hash
  395945892)
 :void (instance rid) (material rid))

(defgmethod
 (rendering-server+instance-geometry-set-material-overlay :class
  'rendering-server :bind "instance_geometry_set_material_overlay" :hash
  395945892)
 :void (instance rid) (material rid))

(defgmethod
 (rendering-server+instance-geometry-set-visibility-range :class
  'rendering-server :bind "instance_geometry_set_visibility_range" :hash
  4263925858)
 :void (instance rid) (min float) (max float) (min-margin float)
 (max-margin float) (fade-mode rendering-server+visibility-range-fade-mode))

(defgmethod
 (rendering-server+instance-geometry-set-lightmap :class 'rendering-server
  :bind "instance_geometry_set_lightmap" :hash 536974962)
 :void (instance rid) (lightmap rid) (lightmap-uv-scale rect-2)
 (lightmap-slice int))

(defgmethod
 (rendering-server+instance-geometry-set-lod-bias :class 'rendering-server
  :bind "instance_geometry_set_lod_bias" :hash 1794382983)
 :void (instance rid) (lod-bias float))

(defgmethod
 (rendering-server+instance-geometry-set-shader-parameter :class
  'rendering-server :bind "instance_geometry_set_shader_parameter" :hash
  3477296213)
 :void (instance rid) (parameter string-name) (value variant))

(defgmethod
 (rendering-server+instance-geometry-get-shader-parameter :class
  'rendering-server :bind "instance_geometry_get_shader_parameter" :hash
  2621281810)
 variant (instance rid) (parameter string-name))

(defgmethod
 (rendering-server+instance-geometry-get-shader-parameter-default-value :class
  'rendering-server :bind
  "instance_geometry_get_shader_parameter_default_value" :hash 2621281810)
 variant (instance rid) (parameter string-name))

(defgmethod
 (rendering-server+instance-geometry-get-shader-parameter-list :class
  'rendering-server :bind "instance_geometry_get_shader_parameter_list" :hash
  2684255073)
 array (instance rid))

(defgmethod
 (rendering-server+instances-cull-aabb :class 'rendering-server :bind
  "instances_cull_aabb" :hash 2570105777)
 packed-int-64array (aabb aabb) (scenario rid))

(defgmethod
 (rendering-server+instances-cull-ray :class 'rendering-server :bind
  "instances_cull_ray" :hash 2208759584)
 packed-int-64array (from vector-3) (to vector-3) (scenario rid))

(defgmethod
 (rendering-server+instances-cull-convex :class 'rendering-server :bind
  "instances_cull_convex" :hash 2488539944)
 packed-int-64array (convex array) (scenario rid))

(defgmethod
 (rendering-server+bake-render-uv2 :class 'rendering-server :bind
  "bake_render_uv2" :hash 1904608558)
 array (base rid) (material-overrides array) (image-size vector-2i))

(defgmethod
 (rendering-server+canvas-create :class 'rendering-server :bind "canvas_create"
  :hash 529393457)
 rid)

(defgmethod
 (rendering-server+canvas-set-item-mirroring :class 'rendering-server :bind
  "canvas_set_item_mirroring" :hash 2343975398)
 :void (canvas rid) (item rid) (mirroring vector-2))

(defgmethod
 (rendering-server+canvas-set-item-repeat :class 'rendering-server :bind
  "canvas_set_item_repeat" :hash 1739512717)
 :void (item rid) (repeat-size vector-2) (repeat-times int))

(defgmethod
 (rendering-server+canvas-set-modulate :class 'rendering-server :bind
  "canvas_set_modulate" :hash 2948539648)
 :void (canvas rid) (color color))

(defgmethod
 (rendering-server+canvas-set-disable-scale :class 'rendering-server :bind
  "canvas_set_disable_scale" :hash 2586408642)
 :void (disable bool))

(defgmethod
 (rendering-server+canvas-texture-create :class 'rendering-server :bind
  "canvas_texture_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+canvas-texture-set-channel :class 'rendering-server :bind
  "canvas_texture_set_channel" :hash 3822119138)
 :void (canvas-texture rid) (channel rendering-server+canvas-texture-channel)
 (texture rid))

(defgmethod
 (rendering-server+canvas-texture-set-shading-parameters :class
  'rendering-server :bind "canvas_texture_set_shading_parameters" :hash
  2124967469)
 :void (canvas-texture rid) (base-color color) (shininess float))

(defgmethod
 (rendering-server+canvas-texture-set-texture-filter :class 'rendering-server
  :bind "canvas_texture_set_texture_filter" :hash 1155129294)
 :void (canvas-texture rid)
 (filter rendering-server+canvas-item-texture-filter))

(defgmethod
 (rendering-server+canvas-texture-set-texture-repeat :class 'rendering-server
  :bind "canvas_texture_set_texture_repeat" :hash 1652956681)
 :void (canvas-texture rid)
 (repeat rendering-server+canvas-item-texture-repeat))

(defgmethod
 (rendering-server+canvas-item-create :class 'rendering-server :bind
  "canvas_item_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+canvas-item-set-parent :class 'rendering-server :bind
  "canvas_item_set_parent" :hash 395945892)
 :void (item rid) (parent rid))

(defgmethod
 (rendering-server+canvas-item-set-default-texture-filter :class
  'rendering-server :bind "canvas_item_set_default_texture_filter" :hash
  1155129294)
 :void (item rid) (filter rendering-server+canvas-item-texture-filter))

(defgmethod
 (rendering-server+canvas-item-set-default-texture-repeat :class
  'rendering-server :bind "canvas_item_set_default_texture_repeat" :hash
  1652956681)
 :void (item rid) (repeat rendering-server+canvas-item-texture-repeat))

(defgmethod
 (rendering-server+canvas-item-set-visible :class 'rendering-server :bind
  "canvas_item_set_visible" :hash 1265174801)
 :void (item rid) (visible bool))

(defgmethod
 (rendering-server+canvas-item-set-light-mask :class 'rendering-server :bind
  "canvas_item_set_light_mask" :hash 3411492887)
 :void (item rid) (mask int))

(defgmethod
 (rendering-server+canvas-item-set-visibility-layer :class 'rendering-server
  :bind "canvas_item_set_visibility_layer" :hash 3411492887)
 :void (item rid) (visibility-layer int))

(defgmethod
 (rendering-server+canvas-item-set-transform :class 'rendering-server :bind
  "canvas_item_set_transform" :hash 1246044741)
 :void (item rid) (transform transform-2d))

(defgmethod
 (rendering-server+canvas-item-set-clip :class 'rendering-server :bind
  "canvas_item_set_clip" :hash 1265174801)
 :void (item rid) (clip bool))

(defgmethod
 (rendering-server+canvas-item-set-distance-field-mode :class 'rendering-server
  :bind "canvas_item_set_distance_field_mode" :hash 1265174801)
 :void (item rid) (enabled bool))

(defgmethod
 (rendering-server+canvas-item-set-custom-rect :class 'rendering-server :bind
  "canvas_item_set_custom_rect" :hash 1333997032)
 :void (item rid) (use-custom-rect bool) (rect rect-2))

(defgmethod
 (rendering-server+canvas-item-set-modulate :class 'rendering-server :bind
  "canvas_item_set_modulate" :hash 2948539648)
 :void (item rid) (color color))

(defgmethod
 (rendering-server+canvas-item-set-self-modulate :class 'rendering-server :bind
  "canvas_item_set_self_modulate" :hash 2948539648)
 :void (item rid) (color color))

(defgmethod
 (rendering-server+canvas-item-set-draw-behind-parent :class 'rendering-server
  :bind "canvas_item_set_draw_behind_parent" :hash 1265174801)
 :void (item rid) (enabled bool))

(defgmethod
 (rendering-server+canvas-item-set-interpolated :class 'rendering-server :bind
  "canvas_item_set_interpolated" :hash 1265174801)
 :void (item rid) (interpolated bool))

(defgmethod
 (rendering-server+canvas-item-reset-physics-interpolation :class
  'rendering-server :bind "canvas_item_reset_physics_interpolation" :hash
  2722037293)
 :void (item rid))

(defgmethod
 (rendering-server+canvas-item-transform-physics-interpolation :class
  'rendering-server :bind "canvas_item_transform_physics_interpolation" :hash
  1246044741)
 :void (item rid) (transform transform-2d))

(defgmethod
 (rendering-server+canvas-item-add-line :class 'rendering-server :bind
  "canvas_item_add_line" :hash 1819681853)
 :void (item rid) (from vector-2) (to vector-2) (color color) (width float)
 (antialiased bool))

(defgmethod
 (rendering-server+canvas-item-add-polyline :class 'rendering-server :bind
  "canvas_item_add_polyline" :hash 3098767073)
 :void (item rid) (points packed-vector-2array) (colors packed-color-array)
 (width float) (antialiased bool))

(defgmethod
 (rendering-server+canvas-item-add-multiline :class 'rendering-server :bind
  "canvas_item_add_multiline" :hash 3098767073)
 :void (item rid) (points packed-vector-2array) (colors packed-color-array)
 (width float) (antialiased bool))

(defgmethod
 (rendering-server+canvas-item-add-rect :class 'rendering-server :bind
  "canvas_item_add_rect" :hash 3523446176)
 :void (item rid) (rect rect-2) (color color) (antialiased bool))

(defgmethod
 (rendering-server+canvas-item-add-circle :class 'rendering-server :bind
  "canvas_item_add_circle" :hash 333077949)
 :void (item rid) (pos vector-2) (radius float) (color color)
 (antialiased bool))

(defgmethod
 (rendering-server+canvas-item-add-ellipse :class 'rendering-server :bind
  "canvas_item_add_ellipse" :hash 4188642757)
 :void (item rid) (pos vector-2) (major float) (minor float) (color color)
 (antialiased bool))

(defgmethod
 (rendering-server+canvas-item-add-texture-rect :class 'rendering-server :bind
  "canvas_item_add_texture_rect" :hash 324864032)
 :void (item rid) (rect rect-2) (texture rid) (tile bool) (modulate color)
 (transpose bool))

(defgmethod
 (rendering-server+canvas-item-add-msdf-texture-rect-region :class
  'rendering-server :bind "canvas_item_add_msdf_texture_rect_region" :hash
  97408773)
 :void (item rid) (rect rect-2) (texture rid) (src-rect rect-2)
 (modulate color) (outline-size int) (px-range float) (scale float))

(defgmethod
 (rendering-server+canvas-item-add-lcd-texture-rect-region :class
  'rendering-server :bind "canvas_item_add_lcd_texture_rect_region" :hash
  359793297)
 :void (item rid) (rect rect-2) (texture rid) (src-rect rect-2)
 (modulate color))

(defgmethod
 (rendering-server+canvas-item-add-texture-rect-region :class 'rendering-server
  :bind "canvas_item_add_texture_rect_region" :hash 485157892)
 :void (item rid) (rect rect-2) (texture rid) (src-rect rect-2)
 (modulate color) (transpose bool) (clip-uv bool))

(defgmethod
 (rendering-server+canvas-item-add-nine-patch :class 'rendering-server :bind
  "canvas_item_add_nine_patch" :hash 389957886)
 :void (item rid) (rect rect-2) (source rect-2) (texture rid)
 (topleft vector-2) (bottomright vector-2)
 (x-axis-mode rendering-server+nine-patch-axis-mode)
 (y-axis-mode rendering-server+nine-patch-axis-mode) (draw-center bool)
 (modulate color))

(defgmethod
 (rendering-server+canvas-item-add-primitive :class 'rendering-server :bind
  "canvas_item_add_primitive" :hash 3731601077)
 :void (item rid) (points packed-vector-2array) (colors packed-color-array)
 (uvs packed-vector-2array) (texture rid))

(defgmethod
 (rendering-server+canvas-item-add-polygon :class 'rendering-server :bind
  "canvas_item_add_polygon" :hash 3580000528)
 :void (item rid) (points packed-vector-2array) (colors packed-color-array)
 (uvs packed-vector-2array) (texture rid))

(defgmethod
 (rendering-server+canvas-item-add-triangle-array :class 'rendering-server
  :bind "canvas_item_add_triangle_array" :hash 660261329)
 :void (item rid) (indices packed-int-32array) (points packed-vector-2array)
 (colors packed-color-array) (uvs packed-vector-2array)
 (bones packed-int-32array) (weights packed-float-32array) (texture rid)
 (count int))

(defgmethod
 (rendering-server+canvas-item-add-mesh :class 'rendering-server :bind
  "canvas_item_add_mesh" :hash 316450961)
 :void (item rid) (mesh rid) (transform transform-2d) (modulate color)
 (texture rid))

(defgmethod
 (rendering-server+canvas-item-add-multimesh :class 'rendering-server :bind
  "canvas_item_add_multimesh" :hash 2131855138)
 :void (item rid) (mesh rid) (texture rid))

(defgmethod
 (rendering-server+canvas-item-add-particles :class 'rendering-server :bind
  "canvas_item_add_particles" :hash 2575754278)
 :void (item rid) (particles rid) (texture rid))

(defgmethod
 (rendering-server+canvas-item-add-set-transform :class 'rendering-server :bind
  "canvas_item_add_set_transform" :hash 1246044741)
 :void (item rid) (transform transform-2d))

(defgmethod
 (rendering-server+canvas-item-add-clip-ignore :class 'rendering-server :bind
  "canvas_item_add_clip_ignore" :hash 1265174801)
 :void (item rid) (ignore bool))

(defgmethod
 (rendering-server+canvas-item-add-animation-slice :class 'rendering-server
  :bind "canvas_item_add_animation_slice" :hash 2646834499)
 :void (item rid) (animation-length float) (slice-begin float)
 (slice-end float) (offset float))

(defgmethod
 (rendering-server+canvas-item-set-sort-children-by-y :class 'rendering-server
  :bind "canvas_item_set_sort_children_by_y" :hash 1265174801)
 :void (item rid) (enabled bool))

(defgmethod
 (rendering-server+canvas-item-set-z-index :class 'rendering-server :bind
  "canvas_item_set_z_index" :hash 3411492887)
 :void (item rid) (z-index int))

(defgmethod
 (rendering-server+canvas-item-set-z-as-relative-to-parent :class
  'rendering-server :bind "canvas_item_set_z_as_relative_to_parent" :hash
  1265174801)
 :void (item rid) (enabled bool))

(defgmethod
 (rendering-server+canvas-item-set-copy-to-backbuffer :class 'rendering-server
  :bind "canvas_item_set_copy_to_backbuffer" :hash 2429202503)
 :void (item rid) (enabled bool) (rect rect-2))

(defgmethod
 (rendering-server+canvas-item-attach-skeleton :class 'rendering-server :bind
  "canvas_item_attach_skeleton" :hash 395945892)
 :void (item rid) (skeleton rid))

(defgmethod
 (rendering-server+canvas-item-clear :class 'rendering-server :bind
  "canvas_item_clear" :hash 2722037293)
 :void (item rid))

(defgmethod
 (rendering-server+canvas-item-set-draw-index :class 'rendering-server :bind
  "canvas_item_set_draw_index" :hash 3411492887)
 :void (item rid) (index int))

(defgmethod
 (rendering-server+canvas-item-set-material :class 'rendering-server :bind
  "canvas_item_set_material" :hash 395945892)
 :void (item rid) (material rid))

(defgmethod
 (rendering-server+canvas-item-set-use-parent-material :class 'rendering-server
  :bind "canvas_item_set_use_parent_material" :hash 1265174801)
 :void (item rid) (enabled bool))

(defgmethod
 (rendering-server+canvas-item-set-instance-shader-parameter :class
  'rendering-server :bind "canvas_item_set_instance_shader_parameter" :hash
  3477296213)
 :void (instance rid) (parameter string-name) (value variant))

(defgmethod
 (rendering-server+canvas-item-get-instance-shader-parameter :class
  'rendering-server :bind "canvas_item_get_instance_shader_parameter" :hash
  2621281810)
 variant (instance rid) (parameter string-name))

(defgmethod
 (rendering-server+canvas-item-get-instance-shader-parameter-default-value
  :class 'rendering-server :bind
  "canvas_item_get_instance_shader_parameter_default_value" :hash 2621281810)
 variant (instance rid) (parameter string-name))

(defgmethod
 (rendering-server+canvas-item-get-instance-shader-parameter-list :class
  'rendering-server :bind "canvas_item_get_instance_shader_parameter_list"
  :hash 2684255073)
 array (instance rid))

(defgmethod
 (rendering-server+canvas-item-set-visibility-notifier :class 'rendering-server
  :bind "canvas_item_set_visibility_notifier" :hash 3568945579)
 :void (item rid) (enable bool) (area rect-2) (enter-callable callable)
 (exit-callable callable))

(defgmethod
 (rendering-server+canvas-item-set-canvas-group-mode :class 'rendering-server
  :bind "canvas_item_set_canvas_group_mode" :hash 3973586316)
 :void (item rid) (mode rendering-server+canvas-group-mode)
 (clear-margin float) (fit-empty bool) (fit-margin float) (blur-mipmaps bool))

(defgmethod
 (rendering-server+debug-canvas-item-get-rect :class 'rendering-server :bind
  "debug_canvas_item_get_rect" :hash 624227424)
 rect-2 (item rid))

(defgmethod
 (rendering-server+canvas-light-create :class 'rendering-server :bind
  "canvas_light_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+canvas-light-attach-to-canvas :class 'rendering-server :bind
  "canvas_light_attach_to_canvas" :hash 395945892)
 :void (light rid) (canvas rid))

(defgmethod
 (rendering-server+canvas-light-set-enabled :class 'rendering-server :bind
  "canvas_light_set_enabled" :hash 1265174801)
 :void (light rid) (enabled bool))

(defgmethod
 (rendering-server+canvas-light-set-texture-scale :class 'rendering-server
  :bind "canvas_light_set_texture_scale" :hash 1794382983)
 :void (light rid) (scale float))

(defgmethod
 (rendering-server+canvas-light-set-transform :class 'rendering-server :bind
  "canvas_light_set_transform" :hash 1246044741)
 :void (light rid) (transform transform-2d))

(defgmethod
 (rendering-server+canvas-light-set-texture :class 'rendering-server :bind
  "canvas_light_set_texture" :hash 395945892)
 :void (light rid) (texture rid))

(defgmethod
 (rendering-server+canvas-light-set-texture-offset :class 'rendering-server
  :bind "canvas_light_set_texture_offset" :hash 3201125042)
 :void (light rid) (offset vector-2))

(defgmethod
 (rendering-server+canvas-light-set-color :class 'rendering-server :bind
  "canvas_light_set_color" :hash 2948539648)
 :void (light rid) (color color))

(defgmethod
 (rendering-server+canvas-light-set-height :class 'rendering-server :bind
  "canvas_light_set_height" :hash 1794382983)
 :void (light rid) (height float))

(defgmethod
 (rendering-server+canvas-light-set-energy :class 'rendering-server :bind
  "canvas_light_set_energy" :hash 1794382983)
 :void (light rid) (energy float))

(defgmethod
 (rendering-server+canvas-light-set-z-range :class 'rendering-server :bind
  "canvas_light_set_z_range" :hash 4288446313)
 :void (light rid) (min-z int) (max-z int))

(defgmethod
 (rendering-server+canvas-light-set-layer-range :class 'rendering-server :bind
  "canvas_light_set_layer_range" :hash 4288446313)
 :void (light rid) (min-layer int) (max-layer int))

(defgmethod
 (rendering-server+canvas-light-set-item-cull-mask :class 'rendering-server
  :bind "canvas_light_set_item_cull_mask" :hash 3411492887)
 :void (light rid) (mask int))

(defgmethod
 (rendering-server+canvas-light-set-item-shadow-cull-mask :class
  'rendering-server :bind "canvas_light_set_item_shadow_cull_mask" :hash
  3411492887)
 :void (light rid) (mask int))

(defgmethod
 (rendering-server+canvas-light-set-mode :class 'rendering-server :bind
  "canvas_light_set_mode" :hash 2957564891)
 :void (light rid) (mode rendering-server+canvas-light-mode))

(defgmethod
 (rendering-server+canvas-light-set-shadow-enabled :class 'rendering-server
  :bind "canvas_light_set_shadow_enabled" :hash 1265174801)
 :void (light rid) (enabled bool))

(defgmethod
 (rendering-server+canvas-light-set-shadow-filter :class 'rendering-server
  :bind "canvas_light_set_shadow_filter" :hash 393119659)
 :void (light rid) (filter rendering-server+canvas-light-shadow-filter))

(defgmethod
 (rendering-server+canvas-light-set-shadow-color :class 'rendering-server :bind
  "canvas_light_set_shadow_color" :hash 2948539648)
 :void (light rid) (color color))

(defgmethod
 (rendering-server+canvas-light-set-shadow-smooth :class 'rendering-server
  :bind "canvas_light_set_shadow_smooth" :hash 1794382983)
 :void (light rid) (smooth float))

(defgmethod
 (rendering-server+canvas-light-set-blend-mode :class 'rendering-server :bind
  "canvas_light_set_blend_mode" :hash 804895945)
 :void (light rid) (mode rendering-server+canvas-light-blend-mode))

(defgmethod
 (rendering-server+canvas-light-set-interpolated :class 'rendering-server :bind
  "canvas_light_set_interpolated" :hash 1265174801)
 :void (light rid) (interpolated bool))

(defgmethod
 (rendering-server+canvas-light-reset-physics-interpolation :class
  'rendering-server :bind "canvas_light_reset_physics_interpolation" :hash
  2722037293)
 :void (light rid))

(defgmethod
 (rendering-server+canvas-light-transform-physics-interpolation :class
  'rendering-server :bind "canvas_light_transform_physics_interpolation" :hash
  1246044741)
 :void (light rid) (transform transform-2d))

(defgmethod
 (rendering-server+canvas-light-occluder-create :class 'rendering-server :bind
  "canvas_light_occluder_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+canvas-light-occluder-attach-to-canvas :class
  'rendering-server :bind "canvas_light_occluder_attach_to_canvas" :hash
  395945892)
 :void (occluder rid) (canvas rid))

(defgmethod
 (rendering-server+canvas-light-occluder-set-enabled :class 'rendering-server
  :bind "canvas_light_occluder_set_enabled" :hash 1265174801)
 :void (occluder rid) (enabled bool))

(defgmethod
 (rendering-server+canvas-light-occluder-set-polygon :class 'rendering-server
  :bind "canvas_light_occluder_set_polygon" :hash 395945892)
 :void (occluder rid) (polygon rid))

(defgmethod
 (rendering-server+canvas-light-occluder-set-as-sdf-collision :class
  'rendering-server :bind "canvas_light_occluder_set_as_sdf_collision" :hash
  1265174801)
 :void (occluder rid) (enable bool))

(defgmethod
 (rendering-server+canvas-light-occluder-set-transform :class 'rendering-server
  :bind "canvas_light_occluder_set_transform" :hash 1246044741)
 :void (occluder rid) (transform transform-2d))

(defgmethod
 (rendering-server+canvas-light-occluder-set-light-mask :class
  'rendering-server :bind "canvas_light_occluder_set_light_mask" :hash
  3411492887)
 :void (occluder rid) (mask int))

(defgmethod
 (rendering-server+canvas-light-occluder-set-interpolated :class
  'rendering-server :bind "canvas_light_occluder_set_interpolated" :hash
  1265174801)
 :void (occluder rid) (interpolated bool))

(defgmethod
 (rendering-server+canvas-light-occluder-reset-physics-interpolation :class
  'rendering-server :bind "canvas_light_occluder_reset_physics_interpolation"
  :hash 2722037293)
 :void (occluder rid))

(defgmethod
 (rendering-server+canvas-light-occluder-transform-physics-interpolation :class
  'rendering-server :bind
  "canvas_light_occluder_transform_physics_interpolation" :hash 1246044741)
 :void (occluder rid) (transform transform-2d))

(defgmethod
 (rendering-server+canvas-occluder-polygon-create :class 'rendering-server
  :bind "canvas_occluder_polygon_create" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+canvas-occluder-polygon-set-shape :class 'rendering-server
  :bind "canvas_occluder_polygon_set_shape" :hash 2103882027)
 :void (occluder-polygon rid) (shape packed-vector-2array) (closed bool))

(defgmethod
 (rendering-server+canvas-occluder-polygon-set-cull-mode :class
  'rendering-server :bind "canvas_occluder_polygon_set_cull_mode" :hash
  1839404663)
 :void (occluder-polygon rid)
 (mode rendering-server+canvas-occluder-polygon-cull-mode))

(defgmethod
 (rendering-server+canvas-set-shadow-texture-size :class 'rendering-server
  :bind "canvas_set_shadow_texture_size" :hash 1286410249)
 :void (size int))

(defgmethod
 (rendering-server+global-shader-parameter-add :class 'rendering-server :bind
  "global_shader_parameter_add" :hash 463390080)
 :void (name string-name) (type rendering-server+global-shader-parameter-type)
 (default-value variant))

(defgmethod
 (rendering-server+global-shader-parameter-remove :class 'rendering-server
  :bind "global_shader_parameter_remove" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (rendering-server+global-shader-parameter-get-list :class 'rendering-server
  :bind "global_shader_parameter_get_list" :hash 3995934104)
 array)

(defgmethod
 (rendering-server+global-shader-parameter-set :class 'rendering-server :bind
  "global_shader_parameter_set" :hash 3776071444)
 :void (name string-name) (value variant))

(defgmethod
 (rendering-server+global-shader-parameter-set-override :class
  'rendering-server :bind "global_shader_parameter_set_override" :hash
  3776071444)
 :void (name string-name) (value variant))

(defgmethod
 (rendering-server+global-shader-parameter-get :class 'rendering-server :bind
  "global_shader_parameter_get" :hash 2760726917)
 variant (name string-name))

(defgmethod
 (rendering-server+global-shader-parameter-get-type :class 'rendering-server
  :bind "global_shader_parameter_get_type" :hash 1601414142)
 rendering-server+global-shader-parameter-type (name string-name))

(defgmethod
 (rendering-server+free-rid :class 'rendering-server :bind "free_rid" :hash
  2722037293)
 :void (rid rid))

(defgmethod
 (rendering-server+request-frame-drawn-callback :class 'rendering-server :bind
  "request_frame_drawn_callback" :hash 1611583062)
 :void (callable callable))

(defgmethod
 (rendering-server+has-changed :class 'rendering-server :bind "has_changed"
  :hash 36873697)
 bool)

(defgmethod
 (rendering-server+get-rendering-info :class 'rendering-server :bind
  "get_rendering_info" :hash 3763192241)
 int (info rendering-server+rendering-info))

(defgmethod
 (rendering-server+get-video-adapter-name :class 'rendering-server :bind
  "get_video_adapter_name" :hash 201670096)
 string)

(defgmethod
 (rendering-server+get-video-adapter-vendor :class 'rendering-server :bind
  "get_video_adapter_vendor" :hash 201670096)
 string)

(defgmethod
 (rendering-server+get-video-adapter-type :class 'rendering-server :bind
  "get_video_adapter_type" :hash 3099547011)
 rendering-device+device-type)

(defgmethod
 (rendering-server+get-video-adapter-api-version :class 'rendering-server :bind
  "get_video_adapter_api_version" :hash 201670096)
 string)

(defgmethod
 (rendering-server+get-current-rendering-driver-name :class 'rendering-server
  :bind "get_current_rendering_driver_name" :hash 201670096)
 string)

(defgmethod
 (rendering-server+get-current-rendering-method :class 'rendering-server :bind
  "get_current_rendering_method" :hash 201670096)
 string)

(defgmethod
 (rendering-server+make-sphere-mesh :class 'rendering-server :bind
  "make_sphere_mesh" :hash 2251015897)
 rid (latitudes int) (longitudes int) (radius float))

(defgmethod
 (rendering-server+get-test-cube :class 'rendering-server :bind "get_test_cube"
  :hash 529393457)
 rid)

(defgmethod
 (rendering-server+get-test-texture :class 'rendering-server :bind
  "get_test_texture" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+get-white-texture :class 'rendering-server :bind
  "get_white_texture" :hash 529393457)
 rid)

(defgmethod
 (rendering-server+set-boot-image-with-stretch :class 'rendering-server :bind
  "set_boot_image_with_stretch" :hash 1104470771)
 :void (image image) (color color)
 (stretch-mode rendering-server+splash-stretch-mode) (use-filter bool))

(defgmethod
 (rendering-server+set-boot-image :class 'rendering-server :bind
  "set_boot_image" :hash 3759744527)
 :void (image image) (color color) (scale bool) (use-filter bool))

(defgmethod
 (rendering-server+get-default-clear-color :class 'rendering-server :bind
  "get_default_clear_color" :hash 3200896285)
 color)

(defgmethod
 (rendering-server+set-default-clear-color :class 'rendering-server :bind
  "set_default_clear_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (rendering-server+has-os-feature :class 'rendering-server :bind
  "has_os_feature" :hash 3927539163)
 bool (feature string))

(defgmethod
 (rendering-server+set-debug-generate-wireframes :class 'rendering-server :bind
  "set_debug_generate_wireframes" :hash 2586408642)
 :void (generate bool))

(defgmethod
 (rendering-server+is-render-loop-enabled :class 'rendering-server :bind
  "is_render_loop_enabled" :hash 36873697)
 bool)

(defgmethod
 (rendering-server+set-render-loop-enabled :class 'rendering-server :bind
  "set_render_loop_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (rendering-server+get-frame-setup-time-cpu :class 'rendering-server :bind
  "get_frame_setup_time_cpu" :hash 1740695150)
 float)

(defgmethod
 (rendering-server+force-sync :class 'rendering-server :bind "force_sync" :hash
  3218959716)
 :void)

(defgmethod
 (rendering-server+force-draw :class 'rendering-server :bind "force_draw" :hash
  1076185472)
 :void (swap-buffers bool) (frame-step float))

(defgmethod
 (rendering-server+get-rendering-device :class 'rendering-server :bind
  "get_rendering_device" :hash 1405107940)
 rendering-device)

(defgmethod
 (rendering-server+create-local-rendering-device :class 'rendering-server :bind
  "create_local_rendering_device" :hash 1405107940)
 rendering-device)

(defgmethod
 (rendering-server+is-on-render-thread :class 'rendering-server :bind
  "is_on_render_thread" :hash 2240911060)
 bool)

(defgmethod
 (rendering-server+call-on-render-thread :class 'rendering-server :bind
  "call_on_render_thread" :hash 1611583062)
 :void (callable callable))

(defgmethod
 (rendering-server+has-feature :class 'rendering-server :bind "has_feature"
  :hash 598462696)
 bool (feature rendering-server+features))