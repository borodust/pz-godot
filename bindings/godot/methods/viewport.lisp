(common-lisp:in-package :%godot)


(defgmethod
 (viewport+set-world-2d :class 'viewport :bind "set_world_2d" :hash 2736080068)
 :void (world-2d world-2d))

(defgmethod
 (viewport+get-world-2d :class 'viewport :bind "get_world_2d" :hash 2339128592)
 world-2d)

(defgmethod
 (viewport+find-world-2d :class 'viewport :bind "find_world_2d" :hash
  2339128592)
 world-2d)

(defgmethod
 (viewport+set-canvas-transform :class 'viewport :bind "set_canvas_transform"
  :hash 2761652528)
 :void (xform transform-2d))

(defgmethod
 (viewport+get-canvas-transform :class 'viewport :bind "get_canvas_transform"
  :hash 3814499831)
 transform-2d)

(defgmethod
 (viewport+set-global-canvas-transform :class 'viewport :bind
  "set_global_canvas_transform" :hash 2761652528)
 :void (xform transform-2d))

(defgmethod
 (viewport+get-global-canvas-transform :class 'viewport :bind
  "get_global_canvas_transform" :hash 3814499831)
 transform-2d)

(defgmethod
 (viewport+get-stretch-transform :class 'viewport :bind "get_stretch_transform"
  :hash 3814499831)
 transform-2d)

(defgmethod
 (viewport+get-final-transform :class 'viewport :bind "get_final_transform"
  :hash 3814499831)
 transform-2d)

(defgmethod
 (viewport+get-screen-transform :class 'viewport :bind "get_screen_transform"
  :hash 3814499831)
 transform-2d)

(defgmethod
 (viewport+get-visible-rect :class 'viewport :bind "get_visible_rect" :hash
  1639390495)
 rect-2)

(defgmethod
 (viewport+set-transparent-background :class 'viewport :bind
  "set_transparent_background" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+has-transparent-background :class 'viewport :bind
  "has_transparent_background" :hash 36873697)
 bool)

(defgmethod
 (viewport+set-use-hdr-2d :class 'viewport :bind "set_use_hdr_2d" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (viewport+is-using-hdr-2d :class 'viewport :bind "is_using_hdr_2d" :hash
  36873697)
 bool)

(defgmethod
 (viewport+set-msaa-2d :class 'viewport :bind "set_msaa_2d" :hash 3330258708)
 :void (msaa viewport+msaa))

(defgmethod
 (viewport+get-msaa-2d :class 'viewport :bind "get_msaa_2d" :hash 2542055527)
 viewport+msaa)

(defgmethod
 (viewport+set-msaa-3d :class 'viewport :bind "set_msaa_3d" :hash 3330258708)
 :void (msaa viewport+msaa))

(defgmethod
 (viewport+get-msaa-3d :class 'viewport :bind "get_msaa_3d" :hash 2542055527)
 viewport+msaa)

(defgmethod
 (viewport+set-screen-space-aa :class 'viewport :bind "set_screen_space_aa"
  :hash 3544169389)
 :void (screen-space-aa viewport+screen-space-aa))

(defgmethod
 (viewport+get-screen-space-aa :class 'viewport :bind "get_screen_space_aa"
  :hash 1390814124)
 viewport+screen-space-aa)

(defgmethod
 (viewport+set-use-taa :class 'viewport :bind "set_use_taa" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+is-using-taa :class 'viewport :bind "is_using_taa" :hash 36873697)
 bool)

(defgmethod
 (viewport+set-use-debanding :class 'viewport :bind "set_use_debanding" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (viewport+is-using-debanding :class 'viewport :bind "is_using_debanding" :hash
  36873697)
 bool)

(defgmethod
 (viewport+set-use-occlusion-culling :class 'viewport :bind
  "set_use_occlusion_culling" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+is-using-occlusion-culling :class 'viewport :bind
  "is_using_occlusion_culling" :hash 36873697)
 bool)

(defgmethod
 (viewport+set-debug-draw :class 'viewport :bind "set_debug_draw" :hash
  1970246205)
 :void (debug-draw viewport+debug-draw))

(defgmethod
 (viewport+get-debug-draw :class 'viewport :bind "get_debug_draw" :hash
  579191299)
 viewport+debug-draw)

(defgmethod
 (viewport+set-use-oversampling :class 'viewport :bind "set_use_oversampling"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+is-using-oversampling :class 'viewport :bind "is_using_oversampling"
  :hash 36873697)
 bool)

(defgmethod
 (viewport+set-oversampling-override :class 'viewport :bind
  "set_oversampling_override" :hash 373806689)
 :void (oversampling float))

(defgmethod
 (viewport+get-oversampling-override :class 'viewport :bind
  "get_oversampling_override" :hash 1740695150)
 float)

(defgmethod
 (viewport+get-oversampling :class 'viewport :bind "get_oversampling" :hash
  1740695150)
 float)

(defgmethod
 (viewport+get-render-info :class 'viewport :bind "get_render_info" :hash
  481977019)
 int (type viewport+render-info-type) (info viewport+render-info))

(defgmethod
 (viewport+get-texture :class 'viewport :bind "get_texture" :hash 1746695840)
 viewport-texture)

(defgmethod
 (viewport+set-physics-object-picking :class 'viewport :bind
  "set_physics_object_picking" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+get-physics-object-picking :class 'viewport :bind
  "get_physics_object_picking" :hash 2240911060)
 bool)

(defgmethod
 (viewport+set-physics-object-picking-sort :class 'viewport :bind
  "set_physics_object_picking_sort" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+get-physics-object-picking-sort :class 'viewport :bind
  "get_physics_object_picking_sort" :hash 2240911060)
 bool)

(defgmethod
 (viewport+set-physics-object-picking-first-only :class 'viewport :bind
  "set_physics_object_picking_first_only" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+get-physics-object-picking-first-only :class 'viewport :bind
  "get_physics_object_picking_first_only" :hash 2240911060)
 bool)

(defgmethod
 (viewport+get-viewport-rid :class 'viewport :bind "get_viewport_rid" :hash
  2944877500)
 rid)

(defgmethod
 (viewport+push-text-input :class 'viewport :bind "push_text_input" :hash
  83702148)
 :void (text string))

(defgmethod
 (viewport+push-input :class 'viewport :bind "push_input" :hash 3644664830)
 :void (event input-event) (in-local-coords bool))

(defgmethod
 (viewport+push-unhandled-input :class 'viewport :bind "push_unhandled_input"
  :hash 3644664830)
 :void (event input-event) (in-local-coords bool))

(defgmethod
 (viewport+notify-mouse-entered :class 'viewport :bind "notify_mouse_entered"
  :hash 3218959716)
 :void)

(defgmethod
 (viewport+notify-mouse-exited :class 'viewport :bind "notify_mouse_exited"
  :hash 3218959716)
 :void)

(defgmethod
 (viewport+get-mouse-position :class 'viewport :bind "get_mouse_position" :hash
  3341600327)
 vector-2)

(defgmethod
 (viewport+warp-mouse :class 'viewport :bind "warp_mouse" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (viewport+update-mouse-cursor-state :class 'viewport :bind
  "update_mouse_cursor_state" :hash 3218959716)
 :void)

(defgmethod
 (viewport+gui-cancel-drag :class 'viewport :bind "gui_cancel_drag" :hash
  3218959716)
 :void)

(defgmethod
 (viewport+gui-get-drag-data :class 'viewport :bind "gui_get_drag_data" :hash
  1214101251)
 variant)

(defgmethod
 (viewport+gui-get-drag-description :class 'viewport :bind
  "gui_get_drag_description" :hash 201670096)
 string)

(defgmethod
 (viewport+gui-set-drag-description :class 'viewport :bind
  "gui_set_drag_description" :hash 83702148)
 :void (description string))

(defgmethod
 (viewport+gui-is-dragging :class 'viewport :bind "gui_is_dragging" :hash
  36873697)
 bool)

(defgmethod
 (viewport+gui-is-drag-successful :class 'viewport :bind
  "gui_is_drag_successful" :hash 36873697)
 bool)

(defgmethod
 (viewport+gui-release-focus :class 'viewport :bind "gui_release_focus" :hash
  3218959716)
 :void)

(defgmethod
 (viewport+gui-get-focus-owner :class 'viewport :bind "gui_get_focus_owner"
  :hash 2783021301)
 control)

(defgmethod
 (viewport+gui-get-hovered-control :class 'viewport :bind
  "gui_get_hovered_control" :hash 2783021301)
 control)

(defgmethod
 (viewport+set-disable-input :class 'viewport :bind "set_disable_input" :hash
  2586408642)
 :void (disable bool))

(defgmethod
 (viewport+is-input-disabled :class 'viewport :bind "is_input_disabled" :hash
  36873697)
 bool)

(defgmethod
 (viewport+set-positional-shadow-atlas-size :class 'viewport :bind
  "set_positional_shadow_atlas_size" :hash 1286410249)
 :void (size int))

(defgmethod
 (viewport+get-positional-shadow-atlas-size :class 'viewport :bind
  "get_positional_shadow_atlas_size" :hash 3905245786)
 int)

(defgmethod
 (viewport+set-positional-shadow-atlas-16-bits :class 'viewport :bind
  "set_positional_shadow_atlas_16_bits" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+get-positional-shadow-atlas-16-bits :class 'viewport :bind
  "get_positional_shadow_atlas_16_bits" :hash 36873697)
 bool)

(defgmethod
 (viewport+set-snap-controls-to-pixels :class 'viewport :bind
  "set_snap_controls_to_pixels" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (viewport+is-snap-controls-to-pixels-enabled :class 'viewport :bind
  "is_snap_controls_to_pixels_enabled" :hash 36873697)
 bool)

(defgmethod
 (viewport+set-snap-2d-transforms-to-pixel :class 'viewport :bind
  "set_snap_2d_transforms_to_pixel" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (viewport+is-snap-2d-transforms-to-pixel-enabled :class 'viewport :bind
  "is_snap_2d_transforms_to_pixel_enabled" :hash 36873697)
 bool)

(defgmethod
 (viewport+set-snap-2d-vertices-to-pixel :class 'viewport :bind
  "set_snap_2d_vertices_to_pixel" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (viewport+is-snap-2d-vertices-to-pixel-enabled :class 'viewport :bind
  "is_snap_2d_vertices_to_pixel_enabled" :hash 36873697)
 bool)

(defgmethod
 (viewport+set-positional-shadow-atlas-quadrant-subdiv :class 'viewport :bind
  "set_positional_shadow_atlas_quadrant_subdiv" :hash 2596956071)
 :void (quadrant int) (subdiv viewport+positional-shadow-atlas-quadrant-subdiv))

(defgmethod
 (viewport+get-positional-shadow-atlas-quadrant-subdiv :class 'viewport :bind
  "get_positional_shadow_atlas_quadrant_subdiv" :hash 2676778355)
 viewport+positional-shadow-atlas-quadrant-subdiv (quadrant int))

(defgmethod
 (viewport+set-input-as-handled :class 'viewport :bind "set_input_as_handled"
  :hash 3218959716)
 :void)

(defgmethod
 (viewport+is-input-handled :class 'viewport :bind "is_input_handled" :hash
  36873697)
 bool)

(defgmethod
 (viewport+set-handle-input-locally :class 'viewport :bind
  "set_handle_input_locally" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+is-handling-input-locally :class 'viewport :bind
  "is_handling_input_locally" :hash 36873697)
 bool)

(defgmethod
 (viewport+set-default-canvas-item-texture-filter :class 'viewport :bind
  "set_default_canvas_item_texture_filter" :hash 2815160100)
 :void (mode viewport+default-canvas-item-texture-filter))

(defgmethod
 (viewport+get-default-canvas-item-texture-filter :class 'viewport :bind
  "get_default_canvas_item_texture_filter" :hash 896601198)
 viewport+default-canvas-item-texture-filter)

(defgmethod
 (viewport+set-embedding-subwindows :class 'viewport :bind
  "set_embedding_subwindows" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+is-embedding-subwindows :class 'viewport :bind
  "is_embedding_subwindows" :hash 36873697)
 bool)

(defgmethod
 (viewport+get-embedded-subwindows :class 'viewport :bind
  "get_embedded_subwindows" :hash 3995934104)
 array)

(defgmethod
 (viewport+set-drag-threshold :class 'viewport :bind "set_drag_threshold" :hash
  1286410249)
 :void (threshold int))

(defgmethod
 (viewport+get-drag-threshold :class 'viewport :bind "get_drag_threshold" :hash
  3905245786)
 int)

(defgmethod
 (viewport+set-canvas-cull-mask :class 'viewport :bind "set_canvas_cull_mask"
  :hash 1286410249)
 :void (mask int))

(defgmethod
 (viewport+get-canvas-cull-mask :class 'viewport :bind "get_canvas_cull_mask"
  :hash 3905245786)
 int)

(defgmethod
 (viewport+set-canvas-cull-mask-bit :class 'viewport :bind
  "set_canvas_cull_mask_bit" :hash 300928843)
 :void (layer int) (enable bool))

(defgmethod
 (viewport+get-canvas-cull-mask-bit :class 'viewport :bind
  "get_canvas_cull_mask_bit" :hash 1116898809)
 bool (layer int))

(defgmethod
 (viewport+set-default-canvas-item-texture-repeat :class 'viewport :bind
  "set_default_canvas_item_texture_repeat" :hash 1658513413)
 :void (mode viewport+default-canvas-item-texture-repeat))

(defgmethod
 (viewport+get-default-canvas-item-texture-repeat :class 'viewport :bind
  "get_default_canvas_item_texture_repeat" :hash 4049774160)
 viewport+default-canvas-item-texture-repeat)

(defgmethod
 (viewport+set-sdf-oversize :class 'viewport :bind "set_sdf_oversize" :hash
  2574159017)
 :void (oversize viewport+sdfoversize))

(defgmethod
 (viewport+get-sdf-oversize :class 'viewport :bind "get_sdf_oversize" :hash
  2631427510)
 viewport+sdfoversize)

(defgmethod
 (viewport+set-sdf-scale :class 'viewport :bind "set_sdf_scale" :hash
  1402773951)
 :void (scale viewport+sdfscale))

(defgmethod
 (viewport+get-sdf-scale :class 'viewport :bind "get_sdf_scale" :hash
  3162688184)
 viewport+sdfscale)

(defgmethod
 (viewport+set-mesh-lod-threshold :class 'viewport :bind
  "set_mesh_lod_threshold" :hash 373806689)
 :void (pixels float))

(defgmethod
 (viewport+get-mesh-lod-threshold :class 'viewport :bind
  "get_mesh_lod_threshold" :hash 1740695150)
 float)

(defgmethod
 (viewport+set-as-audio-listener-2d :class 'viewport :bind
  "set_as_audio_listener_2d" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+is-audio-listener-2d :class 'viewport :bind "is_audio_listener_2d"
  :hash 36873697)
 bool)

(defgmethod
 (viewport+get-audio-listener-2d :class 'viewport :bind "get_audio_listener_2d"
  :hash 1840977180)
 audio-listener-2d)

(defgmethod
 (viewport+get-camera-2d :class 'viewport :bind "get_camera_2d" :hash
  3551466917)
 camera-2d)

(defgmethod
 (viewport+set-world-3d :class 'viewport :bind "set_world_3d" :hash 1400875337)
 :void (world-3d world-3d))

(defgmethod
 (viewport+get-world-3d :class 'viewport :bind "get_world_3d" :hash 317588385)
 world-3d)

(defgmethod
 (viewport+find-world-3d :class 'viewport :bind "find_world_3d" :hash
  317588385)
 world-3d)

(defgmethod
 (viewport+set-use-own-world-3d :class 'viewport :bind "set_use_own_world_3d"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+is-using-own-world-3d :class 'viewport :bind "is_using_own_world_3d"
  :hash 36873697)
 bool)

(defgmethod
 (viewport+get-audio-listener-3d :class 'viewport :bind "get_audio_listener_3d"
  :hash 3472246991)
 audio-listener-3d)

(defgmethod
 (viewport+get-camera-3d :class 'viewport :bind "get_camera_3d" :hash
  2285090890)
 camera-3d)

(defgmethod
 (viewport+set-as-audio-listener-3d :class 'viewport :bind
  "set_as_audio_listener_3d" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (viewport+is-audio-listener-3d :class 'viewport :bind "is_audio_listener_3d"
  :hash 36873697)
 bool)

(defgmethod
 (viewport+set-disable-3d :class 'viewport :bind "set_disable_3d" :hash
  2586408642)
 :void (disable bool))

(defgmethod
 (viewport+is-3d-disabled :class 'viewport :bind "is_3d_disabled" :hash
  36873697)
 bool)

(defgmethod
 (viewport+set-use-xr :class 'viewport :bind "set_use_xr" :hash 2586408642)
 :void (use bool))

(defgmethod
 (viewport+is-using-xr :class 'viewport :bind "is_using_xr" :hash 36873697)
 bool)

(defgmethod
 (viewport+set-scaling-3d-mode :class 'viewport :bind "set_scaling_3d_mode"
  :hash 1531597597)
 :void (scaling-3d-mode viewport+scaling-3dmode))

(defgmethod
 (viewport+get-scaling-3d-mode :class 'viewport :bind "get_scaling_3d_mode"
  :hash 2597660574)
 viewport+scaling-3dmode)

(defgmethod
 (viewport+set-scaling-3d-scale :class 'viewport :bind "set_scaling_3d_scale"
  :hash 373806689)
 :void (scale float))

(defgmethod
 (viewport+get-scaling-3d-scale :class 'viewport :bind "get_scaling_3d_scale"
  :hash 1740695150)
 float)

(defgmethod
 (viewport+set-fsr-sharpness :class 'viewport :bind "set_fsr_sharpness" :hash
  373806689)
 :void (fsr-sharpness float))

(defgmethod
 (viewport+get-fsr-sharpness :class 'viewport :bind "get_fsr_sharpness" :hash
  1740695150)
 float)

(defgmethod
 (viewport+set-texture-mipmap-bias :class 'viewport :bind
  "set_texture_mipmap_bias" :hash 373806689)
 :void (texture-mipmap-bias float))

(defgmethod
 (viewport+get-texture-mipmap-bias :class 'viewport :bind
  "get_texture_mipmap_bias" :hash 1740695150)
 float)

(defgmethod
 (viewport+set-anisotropic-filtering-level :class 'viewport :bind
  "set_anisotropic_filtering_level" :hash 3445583046)
 :void (anisotropic-filtering-level viewport+anisotropic-filtering))

(defgmethod
 (viewport+get-anisotropic-filtering-level :class 'viewport :bind
  "get_anisotropic_filtering_level" :hash 3991528932)
 viewport+anisotropic-filtering)

(defgmethod
 (viewport+set-vrs-mode :class 'viewport :bind "set_vrs_mode" :hash 2749867817)
 :void (mode viewport+vrsmode))

(defgmethod
 (viewport+get-vrs-mode :class 'viewport :bind "get_vrs_mode" :hash 349660525)
 viewport+vrsmode)

(defgmethod
 (viewport+set-vrs-update-mode :class 'viewport :bind "set_vrs_update_mode"
  :hash 3182412319)
 :void (mode viewport+vrsupdate-mode))

(defgmethod
 (viewport+get-vrs-update-mode :class 'viewport :bind "get_vrs_update_mode"
  :hash 2255951583)
 viewport+vrsupdate-mode)

(defgmethod
 (viewport+set-vrs-texture :class 'viewport :bind "set_vrs_texture" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (viewport+get-vrs-texture :class 'viewport :bind "get_vrs_texture" :hash
  3635182373)
 texture-2d)