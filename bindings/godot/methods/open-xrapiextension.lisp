(common-lisp:in-package :%godot)


(defgmethod
 (open-xrapiextension+get-openxr-version :class 'open-xrapiextension :bind
  "get_openxr_version" :hash 2455072627)
 int)

(defgmethod
 (open-xrapiextension+get-instance :class 'open-xrapiextension :bind
  "get_instance" :hash 2455072627)
 int)

(defgmethod
 (open-xrapiextension+get-system-id :class 'open-xrapiextension :bind
  "get_system_id" :hash 2455072627)
 int)

(defgmethod
 (open-xrapiextension+get-session :class 'open-xrapiextension :bind
  "get_session" :hash 2455072627)
 int)

(defgmethod
 (open-xrapiextension+transform-from-pose :class 'open-xrapiextension :bind
  "transform_from_pose" :hash 2963875352)
 transform-3d (pose (:pointer :void)))

(defgmethod
 (open-xrapiextension+xr-result :class 'open-xrapiextension :bind "xr_result"
  :hash 3886436197)
 bool (result int) (format string) (args array))

(defgmethod
 (open-xrapiextension+openxr-is-enabled :class 'open-xrapiextension :bind
  "openxr_is_enabled" :hash 2703660260 :static common-lisp:t)
 bool (check-run-in-editor bool))

(defgmethod
 (open-xrapiextension+get-instance-proc-addr :class 'open-xrapiextension :bind
  "get_instance_proc_addr" :hash 1597066294)
 int (name string))

(defgmethod
 (open-xrapiextension+get-error-string :class 'open-xrapiextension :bind
  "get_error_string" :hash 990163283)
 string (result int))

(defgmethod
 (open-xrapiextension+get-swapchain-format-name :class 'open-xrapiextension
  :bind "get_swapchain_format_name" :hash 990163283)
 string (swapchain-format int))

(defgmethod
 (open-xrapiextension+set-object-name :class 'open-xrapiextension :bind
  "set_object_name" :hash 2285447957)
 :void (object-type int) (object-handle int) (object-name string))

(defgmethod
 (open-xrapiextension+begin-debug-label-region :class 'open-xrapiextension
  :bind "begin_debug_label_region" :hash 83702148)
 :void (label-name string))

(defgmethod
 (open-xrapiextension+end-debug-label-region :class 'open-xrapiextension :bind
  "end_debug_label_region" :hash 3218959716)
 :void)

(defgmethod
 (open-xrapiextension+insert-debug-label :class 'open-xrapiextension :bind
  "insert_debug_label" :hash 83702148)
 :void (label-name string))

(defgmethod
 (open-xrapiextension+get-view-count :class 'open-xrapiextension :bind
  "get_view_count" :hash 3905245786)
 int)

(defgmethod
 (open-xrapiextension+get-view-configuration :class 'open-xrapiextension :bind
  "get_view_configuration" :hash 3905245786)
 int)

(defgmethod
 (open-xrapiextension+is-initialized :class 'open-xrapiextension :bind
  "is_initialized" :hash 2240911060)
 bool)

(defgmethod
 (open-xrapiextension+is-running :class 'open-xrapiextension :bind "is_running"
  :hash 2240911060)
 bool)

(defgmethod
 (open-xrapiextension+set-custom-play-space :class 'open-xrapiextension :bind
  "set_custom_play_space" :hash 1286410249)
 :void (space (:pointer :void)))

(defgmethod
 (open-xrapiextension+get-play-space :class 'open-xrapiextension :bind
  "get_play_space" :hash 2455072627)
 int)

(defgmethod
 (open-xrapiextension+get-predicted-display-time :class 'open-xrapiextension
  :bind "get_predicted_display_time" :hash 2455072627)
 int)

(defgmethod
 (open-xrapiextension+get-next-frame-time :class 'open-xrapiextension :bind
  "get_next_frame_time" :hash 2455072627)
 int)

(defgmethod
 (open-xrapiextension+can-render :class 'open-xrapiextension :bind "can_render"
  :hash 2240911060)
 bool)

(defgmethod
 (open-xrapiextension+find-action :class 'open-xrapiextension :bind
  "find_action" :hash 4106179378)
 rid (name string) (action-set rid))

(defgmethod
 (open-xrapiextension+action-get-handle :class 'open-xrapiextension :bind
  "action_get_handle" :hash 3917799429)
 int (action rid))

(defgmethod
 (open-xrapiextension+get-hand-tracker :class 'open-xrapiextension :bind
  "get_hand_tracker" :hash 3744713108)
 int (hand-index int))

(defgmethod
 (open-xrapiextension+register-composition-layer-provider :class
  'open-xrapiextension :bind "register_composition_layer_provider" :hash
  1477360496)
 :void (extension open-xrextension-wrapper))

(defgmethod
 (open-xrapiextension+unregister-composition-layer-provider :class
  'open-xrapiextension :bind "unregister_composition_layer_provider" :hash
  1477360496)
 :void (extension open-xrextension-wrapper))

(defgmethod
 (open-xrapiextension+register-projection-views-extension :class
  'open-xrapiextension :bind "register_projection_views_extension" :hash
  1477360496)
 :void (extension open-xrextension-wrapper))

(defgmethod
 (open-xrapiextension+unregister-projection-views-extension :class
  'open-xrapiextension :bind "unregister_projection_views_extension" :hash
  1477360496)
 :void (extension open-xrextension-wrapper))

(defgmethod
 (open-xrapiextension+register-frame-info-extension :class 'open-xrapiextension
  :bind "register_frame_info_extension" :hash 1477360496)
 :void (extension open-xrextension-wrapper))

(defgmethod
 (open-xrapiextension+unregister-frame-info-extension :class
  'open-xrapiextension :bind "unregister_frame_info_extension" :hash
  1477360496)
 :void (extension open-xrextension-wrapper))

(defgmethod
 (open-xrapiextension+register-projection-layer-extension :class
  'open-xrapiextension :bind "register_projection_layer_extension" :hash
  1477360496)
 :void (extension open-xrextension-wrapper))

(defgmethod
 (open-xrapiextension+unregister-projection-layer-extension :class
  'open-xrapiextension :bind "unregister_projection_layer_extension" :hash
  1477360496)
 :void (extension open-xrextension-wrapper))

(defgmethod
 (open-xrapiextension+get-render-state-z-near :class 'open-xrapiextension :bind
  "get_render_state_z_near" :hash 191475506)
 float)

(defgmethod
 (open-xrapiextension+get-render-state-z-far :class 'open-xrapiextension :bind
  "get_render_state_z_far" :hash 191475506)
 float)

(defgmethod
 (open-xrapiextension+set-velocity-texture :class 'open-xrapiextension :bind
  "set_velocity_texture" :hash 2722037293)
 :void (render-target rid))

(defgmethod
 (open-xrapiextension+set-velocity-depth-texture :class 'open-xrapiextension
  :bind "set_velocity_depth_texture" :hash 2722037293)
 :void (render-target rid))

(defgmethod
 (open-xrapiextension+set-velocity-target-size :class 'open-xrapiextension
  :bind "set_velocity_target_size" :hash 1130785943)
 :void (target-size vector-2i))

(defgmethod
 (open-xrapiextension+get-supported-swapchain-formats :class
  'open-xrapiextension :bind "get_supported_swapchain_formats" :hash
  3851388692)
 packed-int-64array)

(defgmethod
 (open-xrapiextension+openxr-swapchain-create :class 'open-xrapiextension :bind
  "openxr_swapchain_create" :hash 2162228999)
 int (create-flags int) (usage-flags int) (swapchain-format int) (width int)
 (height int) (sample-count int) (array-size int))

(defgmethod
 (open-xrapiextension+openxr-swapchain-free :class 'open-xrapiextension :bind
  "openxr_swapchain_free" :hash 1286410249)
 :void (swapchain int))

(defgmethod
 (open-xrapiextension+openxr-swapchain-get-swapchain :class
  'open-xrapiextension :bind "openxr_swapchain_get_swapchain" :hash 3744713108)
 int (swapchain int))

(defgmethod
 (open-xrapiextension+openxr-swapchain-acquire :class 'open-xrapiextension
  :bind "openxr_swapchain_acquire" :hash 1286410249)
 :void (swapchain int))

(defgmethod
 (open-xrapiextension+openxr-swapchain-get-image :class 'open-xrapiextension
  :bind "openxr_swapchain_get_image" :hash 937000113)
 rid (swapchain int))

(defgmethod
 (open-xrapiextension+openxr-swapchain-release :class 'open-xrapiextension
  :bind "openxr_swapchain_release" :hash 1286410249)
 :void (swapchain int))

(defgmethod
 (open-xrapiextension+get-projection-layer :class 'open-xrapiextension :bind
  "get_projection_layer" :hash 2455072627)
 int)

(defgmethod
 (open-xrapiextension+set-render-region :class 'open-xrapiextension :bind
  "set_render_region" :hash 1763793166)
 :void (render-region rect-2i))

(defgmethod
 (open-xrapiextension+set-emulate-environment-blend-mode-alpha-blend :class
  'open-xrapiextension :bind "set_emulate_environment_blend_mode_alpha_blend"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (open-xrapiextension+is-environment-blend-mode-alpha-supported :class
  'open-xrapiextension :bind "is_environment_blend_mode_alpha_supported" :hash
  1579290861)
 open-xrapiextension+open-xralpha-blend-mode-support)

(defgmethod
 (open-xrapiextension+update-main-swapchain-size :class 'open-xrapiextension
  :bind "update_main_swapchain_size" :hash 3218959716)
 :void)