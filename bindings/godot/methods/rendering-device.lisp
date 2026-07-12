(common-lisp:in-package :%godot)


(defgmethod
 (rendering-device+texture-create :class 'rendering-device :bind
  "texture_create" :hash 3709173589)
 rid (format rdtexture-format) (view rdtexture-view) (data array))

(defgmethod
 (rendering-device+texture-create-shared :class 'rendering-device :bind
  "texture_create_shared" :hash 3178156134)
 rid (view rdtexture-view) (with-texture rid))

(defgmethod
 (rendering-device+texture-create-shared-from-slice :class 'rendering-device
  :bind "texture_create_shared_from_slice" :hash 1808971279)
 rid (view rdtexture-view) (with-texture rid) (layer int) (mipmap int)
 (mipmaps int) (slice-type rendering-device+texture-slice-type))

(defgmethod
 (rendering-device+texture-create-from-extension :class 'rendering-device :bind
  "texture_create_from_extension" :hash 3732868568)
 rid (type rendering-device+texture-type) (format rendering-device+data-format)
 (samples rendering-device+texture-samples)
 (usage-flags rendering-device+texture-usage-bits) (image int) (width int)
 (height int) (depth int) (layers int) (mipmaps int))

(defgmethod
 (rendering-device+texture-update :class 'rendering-device :bind
  "texture_update" :hash 1349464008)
 error (texture rid) (layer int) (data packed-byte-array))

(defgmethod
 (rendering-device+texture-get-data :class 'rendering-device :bind
  "texture_get_data" :hash 1859412099)
 packed-byte-array (texture rid) (layer int))

(defgmethod
 (rendering-device+texture-get-data-async :class 'rendering-device :bind
  "texture_get_data_async" :hash 498832090)
 error (texture rid) (layer int) (callback callable))

(defgmethod
 (rendering-device+texture-is-format-supported-for-usage :class
  'rendering-device :bind "texture_is_format_supported_for_usage" :hash
  2592520478)
 bool (format rendering-device+data-format)
 (usage-flags rendering-device+texture-usage-bits))

(defgmethod
 (rendering-device+texture-is-shared :class 'rendering-device :bind
  "texture_is_shared" :hash 3521089500)
 bool (texture rid))

(defgmethod
 (rendering-device+texture-is-valid :class 'rendering-device :bind
  "texture_is_valid" :hash 3521089500)
 bool (texture rid))

(defgmethod
 (rendering-device+texture-set-discardable :class 'rendering-device :bind
  "texture_set_discardable" :hash 1265174801)
 :void (texture rid) (discardable bool))

(defgmethod
 (rendering-device+texture-is-discardable :class 'rendering-device :bind
  "texture_is_discardable" :hash 3521089500)
 bool (texture rid))

(defgmethod
 (rendering-device+texture-copy :class 'rendering-device :bind "texture_copy"
  :hash 2859522160)
 error (from-texture rid) (to-texture rid) (from-pos vector-3)
 (to-pos vector-3) (size vector-3) (src-mipmap int) (dst-mipmap int)
 (src-layer int) (dst-layer int))

(defgmethod
 (rendering-device+texture-clear :class 'rendering-device :bind "texture_clear"
  :hash 3477703247)
 error (texture rid) (color color) (base-mipmap int) (mipmap-count int)
 (base-layer int) (layer-count int))

(defgmethod
 (rendering-device+texture-resolve-multisample :class 'rendering-device :bind
  "texture_resolve_multisample" :hash 3181288260)
 error (from-texture rid) (to-texture rid))

(defgmethod
 (rendering-device+texture-get-format :class 'rendering-device :bind
  "texture_get_format" :hash 1374471690)
 rdtexture-format (texture rid))

(defgmethod
 (rendering-device+texture-get-native-handle :class 'rendering-device :bind
  "texture_get_native_handle" :hash 3917799429)
 int (texture rid))

(defgmethod
 (rendering-device+framebuffer-format-create :class 'rendering-device :bind
  "framebuffer_format_create" :hash 697032759)
 int (attachments array) (view-count int))

(defgmethod
 (rendering-device+framebuffer-format-create-multipass :class 'rendering-device
  :bind "framebuffer_format_create_multipass" :hash 2647479094)
 int (attachments array) (passes array) (view-count int))

(defgmethod
 (rendering-device+framebuffer-format-create-empty :class 'rendering-device
  :bind "framebuffer_format_create_empty" :hash 555930169)
 int (samples rendering-device+texture-samples))

(defgmethod
 (rendering-device+framebuffer-format-get-texture-samples :class
  'rendering-device :bind "framebuffer_format_get_texture_samples" :hash
  4223391010)
 rendering-device+texture-samples (format int) (render-pass int))

(defgmethod
 (rendering-device+framebuffer-create :class 'rendering-device :bind
  "framebuffer_create" :hash 3284231055)
 rid (textures array) (validate-with-format int) (view-count int))

(defgmethod
 (rendering-device+framebuffer-create-multipass :class 'rendering-device :bind
  "framebuffer_create_multipass" :hash 1750306695)
 rid (textures array) (passes array) (validate-with-format int)
 (view-count int))

(defgmethod
 (rendering-device+framebuffer-create-empty :class 'rendering-device :bind
  "framebuffer_create_empty" :hash 3058360618)
 rid (size vector-2i) (samples rendering-device+texture-samples)
 (validate-with-format int))

(defgmethod
 (rendering-device+framebuffer-get-format :class 'rendering-device :bind
  "framebuffer_get_format" :hash 3917799429)
 int (framebuffer rid))

(defgmethod
 (rendering-device+framebuffer-is-valid :class 'rendering-device :bind
  "framebuffer_is_valid" :hash 4155700596)
 bool (framebuffer rid))

(defgmethod
 (rendering-device+sampler-create :class 'rendering-device :bind
  "sampler_create" :hash 2327892535)
 rid (state rdsampler-state))

(defgmethod
 (rendering-device+sampler-is-format-supported-for-filter :class
  'rendering-device :bind "sampler_is_format_supported_for_filter" :hash
  2247922238)
 bool (format rendering-device+data-format)
 (sampler-filter rendering-device+sampler-filter))

(defgmethod
 (rendering-device+vertex-buffer-create :class 'rendering-device :bind
  "vertex_buffer_create" :hash 2089548973)
 rid (size-bytes int) (data packed-byte-array)
 (creation-bits rendering-device+buffer-creation-bits))

(defgmethod
 (rendering-device+vertex-format-create :class 'rendering-device :bind
  "vertex_format_create" :hash 1242678479)
 int (vertex-descriptions array))

(defgmethod
 (rendering-device+vertex-array-create :class 'rendering-device :bind
  "vertex_array_create" :hash 3799816279)
 rid (vertex-count int) (vertex-format int) (src-buffers array)
 (offsets packed-int-64array))

(defgmethod
 (rendering-device+index-buffer-create :class 'rendering-device :bind
  "index_buffer_create" :hash 2368684885)
 rid (size-indices int) (format rendering-device+index-buffer-format)
 (data packed-byte-array) (use-restart-indices bool)
 (creation-bits rendering-device+buffer-creation-bits))

(defgmethod
 (rendering-device+index-array-create :class 'rendering-device :bind
  "index_array_create" :hash 2256026069)
 rid (index-buffer rid) (index-offset int) (index-count int))

(defgmethod
 (rendering-device+shader-compile-spirv-from-source :class 'rendering-device
  :bind "shader_compile_spirv_from_source" :hash 1178973306)
 rdshader-spirv (shader-source rdshader-source) (allow-cache bool))

(defgmethod
 (rendering-device+shader-compile-binary-from-spirv :class 'rendering-device
  :bind "shader_compile_binary_from_spirv" :hash 134910450)
 packed-byte-array (spirv-data rdshader-spirv) (name string))

(defgmethod
 (rendering-device+shader-create-from-spirv :class 'rendering-device :bind
  "shader_create_from_spirv" :hash 342949005)
 rid (spirv-data rdshader-spirv) (name string))

(defgmethod
 (rendering-device+shader-create-from-bytecode :class 'rendering-device :bind
  "shader_create_from_bytecode" :hash 1687031350)
 rid (binary-data packed-byte-array) (placeholder-rid rid))

(defgmethod
 (rendering-device+shader-create-placeholder :class 'rendering-device :bind
  "shader_create_placeholder" :hash 529393457)
 rid)

(defgmethod
 (rendering-device+shader-get-vertex-input-attribute-mask :class
  'rendering-device :bind "shader_get_vertex_input_attribute_mask" :hash
  3917799429)
 int (shader rid))

(defgmethod
 (rendering-device+uniform-buffer-create :class 'rendering-device :bind
  "uniform_buffer_create" :hash 2089548973)
 rid (size-bytes int) (data packed-byte-array)
 (creation-bits rendering-device+buffer-creation-bits))

(defgmethod
 (rendering-device+storage-buffer-create :class 'rendering-device :bind
  "storage_buffer_create" :hash 1609052553)
 rid (size-bytes int) (data packed-byte-array)
 (usage rendering-device+storage-buffer-usage)
 (creation-bits rendering-device+buffer-creation-bits))

(defgmethod
 (rendering-device+texture-buffer-create :class 'rendering-device :bind
  "texture_buffer_create" :hash 1470338698)
 rid (size-bytes int) (format rendering-device+data-format)
 (data packed-byte-array))

(defgmethod
 (rendering-device+uniform-set-create :class 'rendering-device :bind
  "uniform_set_create" :hash 2280795797)
 rid (uniforms array) (shader rid) (shader-set int))

(defgmethod
 (rendering-device+uniform-set-is-valid :class 'rendering-device :bind
  "uniform_set_is_valid" :hash 3521089500)
 bool (uniform-set rid))

(defgmethod
 (rendering-device+buffer-copy :class 'rendering-device :bind "buffer_copy"
  :hash 864257779)
 error (src-buffer rid) (dst-buffer rid) (src-offset int) (dst-offset int)
 (size int))

(defgmethod
 (rendering-device+buffer-update :class 'rendering-device :bind "buffer_update"
  :hash 3454956949)
 error (buffer rid) (offset int) (size-bytes int) (data packed-byte-array))

(defgmethod
 (rendering-device+buffer-clear :class 'rendering-device :bind "buffer_clear"
  :hash 2452320800)
 error (buffer rid) (offset int) (size-bytes int))

(defgmethod
 (rendering-device+buffer-get-data :class 'rendering-device :bind
  "buffer_get_data" :hash 3101830688)
 packed-byte-array (buffer rid) (offset-bytes int) (size-bytes int))

(defgmethod
 (rendering-device+buffer-get-data-async :class 'rendering-device :bind
  "buffer_get_data_async" :hash 2370287848)
 error (buffer rid) (callback callable) (offset-bytes int) (size-bytes int))

(defgmethod
 (rendering-device+buffer-get-device-address :class 'rendering-device :bind
  "buffer_get_device_address" :hash 3917799429)
 int (buffer rid))

(defgmethod
 (rendering-device+render-pipeline-create :class 'rendering-device :bind
  "render_pipeline_create" :hash 2385451958)
 rid (shader rid) (framebuffer-format int) (vertex-format int)
 (primitive rendering-device+render-primitive)
 (rasterization-state rdpipeline-rasterization-state)
 (multisample-state rdpipeline-multisample-state)
 (stencil-state rdpipeline-depth-stencil-state)
 (color-blend-state rdpipeline-color-blend-state)
 (dynamic-state-flags rendering-device+pipeline-dynamic-state-flags)
 (for-render-pass int) (specialization-constants array))

(defgmethod
 (rendering-device+render-pipeline-is-valid :class 'rendering-device :bind
  "render_pipeline_is_valid" :hash 3521089500)
 bool (render-pipeline rid))

(defgmethod
 (rendering-device+compute-pipeline-create :class 'rendering-device :bind
  "compute_pipeline_create" :hash 1448838280)
 rid (shader rid) (specialization-constants array))

(defgmethod
 (rendering-device+compute-pipeline-is-valid :class 'rendering-device :bind
  "compute_pipeline_is_valid" :hash 3521089500)
 bool (compute-pipeline rid))

(defgmethod
 (rendering-device+raytracing-pipeline-create :class 'rendering-device :bind
  "raytracing_pipeline_create" :hash 1489129684)
 rid (raygen-shaders array) (miss-shaders array) (hit-groups array)
 (max-trace-recursion-depth int))

(defgmethod
 (rendering-device+raytracing-pipeline-is-valid :class 'rendering-device :bind
  "raytracing_pipeline_is_valid" :hash 3521089500)
 bool (raytracing-pipeline rid))

(defgmethod
 (rendering-device+blas-create :class 'rendering-device :bind "blas_create"
  :hash 1010940044)
 rid (geometries array)
 (flags rendering-device+acceleration-structure-flag-bits))

(defgmethod
 (rendering-device+tlas-create :class 'rendering-device :bind "tlas_create"
  :hash 592780330)
 rid (max-instance-count int)
 (flags rendering-device+acceleration-structure-flag-bits))

(defgmethod
 (rendering-device+blas-build :class 'rendering-device :bind "blas_build" :hash
  813180755)
 error (blas rid))

(defgmethod
 (rendering-device+tlas-build :class 'rendering-device :bind "tlas_build" :hash
  261981775)
 error (tlas rid) (instances array))

(defgmethod
 (rendering-device+hit-sbt-create :class 'rendering-device :bind
  "hit_sbt_create" :hash 2233757277)
 rid (raytracing-pipeline rid) (initial-hit-group-capacity int))

(defgmethod
 (rendering-device+hit-sbt-set-pipeline :class 'rendering-device :bind
  "hit_sbt_set_pipeline" :hash 3181288260)
 error (hit-sbt rid) (raytracing-pipeline rid))

(defgmethod
 (rendering-device+hit-sbt-range-alloc :class 'rendering-device :bind
  "hit_sbt_range_alloc" :hash 2722015314)
 int (hit-sbt rid) (hit-group-count int))

(defgmethod
 (rendering-device+hit-sbt-range-free :class 'rendering-device :bind
  "hit_sbt_range_free" :hash 3804025326)
 error (hit-sbt rid) (range int))

(defgmethod
 (rendering-device+hit-sbt-range-update :class 'rendering-device :bind
  "hit_sbt_range_update" :hash 1332346675)
 error (hit-sbt rid) (range int) (offset int)
 (hit-group-indices packed-int-32array))

(defgmethod
 (rendering-device+screen-get-width :class 'rendering-device :bind
  "screen_get_width" :hash 1591665591)
 int (screen int))

(defgmethod
 (rendering-device+screen-get-height :class 'rendering-device :bind
  "screen_get_height" :hash 1591665591)
 int (screen int))

(defgmethod
 (rendering-device+screen-get-framebuffer-format :class 'rendering-device :bind
  "screen_get_framebuffer_format" :hash 1591665591)
 int (screen int))

(defgmethod
 (rendering-device+draw-list-begin-for-screen :class 'rendering-device :bind
  "draw_list_begin_for_screen" :hash 3988079995)
 int (screen int) (clear-color color))

(defgmethod
 (rendering-device+draw-list-begin :class 'rendering-device :bind
  "draw_list_begin" :hash 1317926357)
 int (framebuffer rid) (draw-flags rendering-device+draw-flags)
 (clear-color-values packed-color-array) (clear-depth-value float)
 (clear-stencil-value int) (region rect-2) (breadcrumb int))

(defgmethod
 (rendering-device+draw-list-begin-split :class 'rendering-device :bind
  "draw_list_begin_split" :hash 2406300660)
 packed-int-64array (framebuffer rid) (splits int)
 (initial-color-action rendering-device+initial-action)
 (final-color-action rendering-device+final-action)
 (initial-depth-action rendering-device+initial-action)
 (final-depth-action rendering-device+final-action)
 (clear-color-values packed-color-array) (clear-depth float)
 (clear-stencil int) (region rect-2) (storage-textures array))

(defgmethod
 (rendering-device+draw-list-set-blend-constants :class 'rendering-device :bind
  "draw_list_set_blend_constants" :hash 2878471219)
 :void (draw-list int) (color color))

(defgmethod
 (rendering-device+draw-list-bind-render-pipeline :class 'rendering-device
  :bind "draw_list_bind_render_pipeline" :hash 4040184819)
 :void (draw-list int) (render-pipeline rid))

(defgmethod
 (rendering-device+draw-list-bind-uniform-set :class 'rendering-device :bind
  "draw_list_bind_uniform_set" :hash 749655778)
 :void (draw-list int) (uniform-set rid) (set-index int))

(defgmethod
 (rendering-device+draw-list-bind-vertex-array :class 'rendering-device :bind
  "draw_list_bind_vertex_array" :hash 4040184819)
 :void (draw-list int) (vertex-array rid))

(defgmethod
 (rendering-device+draw-list-bind-vertex-buffers-format :class
  'rendering-device :bind "draw_list_bind_vertex_buffers_format" :hash
  2008628980)
 :void (draw-list int) (vertex-format int) (vertex-count int)
 (vertex-buffers array) (offsets packed-int-64array))

(defgmethod
 (rendering-device+draw-list-bind-index-array :class 'rendering-device :bind
  "draw_list_bind_index_array" :hash 4040184819)
 :void (draw-list int) (index-array rid))

(defgmethod
 (rendering-device+draw-list-set-push-constant :class 'rendering-device :bind
  "draw_list_set_push_constant" :hash 2772371345)
 :void (draw-list int) (buffer packed-byte-array) (size-bytes int))

(defgmethod
 (rendering-device+draw-list-draw :class 'rendering-device :bind
  "draw_list_draw" :hash 4230067973)
 :void (draw-list int) (use-indices bool) (instances int)
 (procedural-vertex-count int))

(defgmethod
 (rendering-device+draw-list-draw-indirect :class 'rendering-device :bind
  "draw_list_draw_indirect" :hash 1092133571)
 :void (draw-list int) (use-indices bool) (buffer rid) (offset int)
 (draw-count int) (stride int))

(defgmethod
 (rendering-device+draw-list-enable-scissor :class 'rendering-device :bind
  "draw_list_enable_scissor" :hash 244650101)
 :void (draw-list int) (rect rect-2))

(defgmethod
 (rendering-device+draw-list-disable-scissor :class 'rendering-device :bind
  "draw_list_disable_scissor" :hash 1286410249)
 :void (draw-list int))

(defgmethod
 (rendering-device+draw-list-switch-to-next-pass :class 'rendering-device :bind
  "draw_list_switch_to_next_pass" :hash 2455072627)
 int)

(defgmethod
 (rendering-device+draw-list-switch-to-next-pass-split :class 'rendering-device
  :bind "draw_list_switch_to_next_pass_split" :hash 2865087369)
 packed-int-64array (splits int))

(defgmethod
 (rendering-device+draw-list-end :class 'rendering-device :bind "draw_list_end"
  :hash 3218959716)
 :void)

(defgmethod
 (rendering-device+compute-list-begin :class 'rendering-device :bind
  "compute_list_begin" :hash 2455072627)
 int)

(defgmethod
 (rendering-device+compute-list-bind-compute-pipeline :class 'rendering-device
  :bind "compute_list_bind_compute_pipeline" :hash 4040184819)
 :void (compute-list int) (compute-pipeline rid))

(defgmethod
 (rendering-device+compute-list-set-push-constant :class 'rendering-device
  :bind "compute_list_set_push_constant" :hash 2772371345)
 :void (compute-list int) (buffer packed-byte-array) (size-bytes int))

(defgmethod
 (rendering-device+compute-list-bind-uniform-set :class 'rendering-device :bind
  "compute_list_bind_uniform_set" :hash 749655778)
 :void (compute-list int) (uniform-set rid) (set-index int))

(defgmethod
 (rendering-device+compute-list-dispatch :class 'rendering-device :bind
  "compute_list_dispatch" :hash 4275841770)
 :void (compute-list int) (x-groups int) (y-groups int) (z-groups int))

(defgmethod
 (rendering-device+compute-list-dispatch-indirect :class 'rendering-device
  :bind "compute_list_dispatch_indirect" :hash 749655778)
 :void (compute-list int) (buffer rid) (offset int))

(defgmethod
 (rendering-device+compute-list-add-barrier :class 'rendering-device :bind
  "compute_list_add_barrier" :hash 1286410249)
 :void (compute-list int))

(defgmethod
 (rendering-device+compute-list-end :class 'rendering-device :bind
  "compute_list_end" :hash 3218959716)
 :void)

(defgmethod
 (rendering-device+raytracing-list-begin :class 'rendering-device :bind
  "raytracing_list_begin" :hash 2455072627)
 int)

(defgmethod
 (rendering-device+raytracing-list-bind-raytracing-pipeline :class
  'rendering-device :bind "raytracing_list_bind_raytracing_pipeline" :hash
  4040184819)
 :void (raytracing-list int) (raytracing-pipeline rid))

(defgmethod
 (rendering-device+raytracing-list-set-push-constant :class 'rendering-device
  :bind "raytracing_list_set_push_constant" :hash 2772371345)
 :void (raytracing-list int) (buffer packed-byte-array) (size-bytes int))

(defgmethod
 (rendering-device+raytracing-list-bind-uniform-set :class 'rendering-device
  :bind "raytracing_list_bind_uniform_set" :hash 749655778)
 :void (raytracing-list int) (uniform-set rid) (set-index int))

(defgmethod
 (rendering-device+raytracing-list-trace-rays :class 'rendering-device :bind
  "raytracing_list_trace_rays" :hash 2559472681)
 :void (raytracing-list int) (raygen-shader-index int) (hit-sbt rid)
 (width int) (height int) (depth int))

(defgmethod
 (rendering-device+raytracing-list-end :class 'rendering-device :bind
  "raytracing_list_end" :hash 3218959716)
 :void)

(defgmethod
 (rendering-device+free-rid :class 'rendering-device :bind "free_rid" :hash
  2722037293)
 :void (rid rid))

(defgmethod
 (rendering-device+capture-timestamp :class 'rendering-device :bind
  "capture_timestamp" :hash 83702148)
 :void (name string))

(defgmethod
 (rendering-device+get-captured-timestamps-count :class 'rendering-device :bind
  "get_captured_timestamps_count" :hash 3905245786)
 int)

(defgmethod
 (rendering-device+get-captured-timestamps-frame :class 'rendering-device :bind
  "get_captured_timestamps_frame" :hash 3905245786)
 int)

(defgmethod
 (rendering-device+get-captured-timestamp-gpu-time :class 'rendering-device
  :bind "get_captured_timestamp_gpu_time" :hash 923996154)
 int (index int))

(defgmethod
 (rendering-device+get-captured-timestamp-cpu-time :class 'rendering-device
  :bind "get_captured_timestamp_cpu_time" :hash 923996154)
 int (index int))

(defgmethod
 (rendering-device+get-captured-timestamp-name :class 'rendering-device :bind
  "get_captured_timestamp_name" :hash 844755477)
 string (index int))

(defgmethod
 (rendering-device+has-feature :class 'rendering-device :bind "has_feature"
  :hash 1772728326)
 bool (feature rendering-device+features))

(defgmethod
 (rendering-device+limit-get :class 'rendering-device :bind "limit_get" :hash
  1559202131)
 int (limit rendering-device+limit))

(defgmethod
 (rendering-device+get-frame-delay :class 'rendering-device :bind
  "get_frame_delay" :hash 3905245786)
 int)

(defgmethod
 (rendering-device+submit :class 'rendering-device :bind "submit" :hash
  3218959716)
 :void)

(defgmethod
 (rendering-device+sync :class 'rendering-device :bind "sync" :hash 3218959716)
 :void)

(defgmethod
 (rendering-device+barrier :class 'rendering-device :bind "barrier" :hash
  3718155691)
 :void (from rendering-device+barrier-mask) (to rendering-device+barrier-mask))

(defgmethod
 (rendering-device+full-barrier :class 'rendering-device :bind "full_barrier"
  :hash 3218959716)
 :void)

(defgmethod
 (rendering-device+create-local-device :class 'rendering-device :bind
  "create_local_device" :hash 2846302423)
 rendering-device)

(defgmethod
 (rendering-device+set-resource-name :class 'rendering-device :bind
  "set_resource_name" :hash 2726140452)
 :void (id rid) (name string))

(defgmethod
 (rendering-device+draw-command-begin-label :class 'rendering-device :bind
  "draw_command_begin_label" :hash 1636512886)
 :void (name string) (color color))

(defgmethod
 (rendering-device+draw-command-insert-label :class 'rendering-device :bind
  "draw_command_insert_label" :hash 1636512886)
 :void (name string) (color color))

(defgmethod
 (rendering-device+draw-command-end-label :class 'rendering-device :bind
  "draw_command_end_label" :hash 3218959716)
 :void)

(defgmethod
 (rendering-device+get-device-vendor-name :class 'rendering-device :bind
  "get_device_vendor_name" :hash 201670096)
 string)

(defgmethod
 (rendering-device+get-device-name :class 'rendering-device :bind
  "get_device_name" :hash 201670096)
 string)

(defgmethod
 (rendering-device+get-device-pipeline-cache-uuid :class 'rendering-device
  :bind "get_device_pipeline_cache_uuid" :hash 201670096)
 string)

(defgmethod
 (rendering-device+get-memory-usage :class 'rendering-device :bind
  "get_memory_usage" :hash 251690689)
 int (type rendering-device+memory-type))

(defgmethod
 (rendering-device+get-driver-resource :class 'rendering-device :bind
  "get_driver_resource" :hash 501815484)
 int (resource rendering-device+driver-resource) (rid rid) (index int))

(defgmethod
 (rendering-device+get-perf-report :class 'rendering-device :bind
  "get_perf_report" :hash 201670096)
 string)

(defgmethod
 (rendering-device+get-driver-and-device-memory-report :class 'rendering-device
  :bind "get_driver_and_device_memory_report" :hash 201670096)
 string)

(defgmethod
 (rendering-device+get-tracked-object-name :class 'rendering-device :bind
  "get_tracked_object_name" :hash 844755477)
 string (type-index int))

(defgmethod
 (rendering-device+get-tracked-object-type-count :class 'rendering-device :bind
  "get_tracked_object_type_count" :hash 3905245786)
 int)

(defgmethod
 (rendering-device+get-driver-total-memory :class 'rendering-device :bind
  "get_driver_total_memory" :hash 3905245786)
 int)

(defgmethod
 (rendering-device+get-driver-allocation-count :class 'rendering-device :bind
  "get_driver_allocation_count" :hash 3905245786)
 int)

(defgmethod
 (rendering-device+get-driver-memory-by-object-type :class 'rendering-device
  :bind "get_driver_memory_by_object_type" :hash 923996154)
 int (type int))

(defgmethod
 (rendering-device+get-driver-allocs-by-object-type :class 'rendering-device
  :bind "get_driver_allocs_by_object_type" :hash 923996154)
 int (type int))

(defgmethod
 (rendering-device+get-device-total-memory :class 'rendering-device :bind
  "get_device_total_memory" :hash 3905245786)
 int)

(defgmethod
 (rendering-device+get-device-allocation-count :class 'rendering-device :bind
  "get_device_allocation_count" :hash 3905245786)
 int)

(defgmethod
 (rendering-device+get-device-memory-by-object-type :class 'rendering-device
  :bind "get_device_memory_by_object_type" :hash 923996154)
 int (type int))

(defgmethod
 (rendering-device+get-device-allocs-by-object-type :class 'rendering-device
  :bind "get_device_allocs_by_object_type" :hash 923996154)
 int (type int))