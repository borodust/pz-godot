(common-lisp:in-package :%godot)


(defgmethod
 (open-xrextension-wrapper+%get-requested-extensions :class
  'open-xrextension-wrapper :bind "_get_requested_extensions" :hash 3554694381
  :virtual common-lisp:t)
 dictionary (xr-version int))

(defgmethod
 (open-xrextension-wrapper+%set-system-properties-and-get-next-pointer :class
  'open-xrextension-wrapper :bind "_set_system_properties_and_get_next_pointer"
  :hash 3744713108 :virtual common-lisp:t)
 int (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-instance-create-info-and-get-next-pointer
  :class 'open-xrextension-wrapper :bind
  "_set_instance_create_info_and_get_next_pointer" :hash 50157827 :virtual
  common-lisp:t)
 int (xr-version int) (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-session-create-and-get-next-pointer :class
  'open-xrextension-wrapper :bind "_set_session_create_and_get_next_pointer"
  :hash 3744713108 :virtual common-lisp:t)
 int (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-swapchain-create-info-and-get-next-pointer
  :class 'open-xrextension-wrapper :bind
  "_set_swapchain_create_info_and_get_next_pointer" :hash 3744713108 :virtual
  common-lisp:t)
 int (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-hand-joint-locations-and-get-next-pointer
  :class 'open-xrextension-wrapper :bind
  "_set_hand_joint_locations_and_get_next_pointer" :hash 50157827 :virtual
  common-lisp:t)
 int (hand-index int) (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-projection-views-and-get-next-pointer :class
  'open-xrextension-wrapper :bind "_set_projection_views_and_get_next_pointer"
  :hash 50157827 :virtual common-lisp:t)
 int (view-index int) (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-frame-wait-info-and-get-next-pointer :class
  'open-xrextension-wrapper :bind "_set_frame_wait_info_and_get_next_pointer"
  :hash 3744713108 :virtual common-lisp:t)
 int (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-frame-end-info-and-get-next-pointer :class
  'open-xrextension-wrapper :bind "_set_frame_end_info_and_get_next_pointer"
  :hash 3744713108 :virtual common-lisp:t)
 int (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-projection-layer-and-get-next-pointer :class
  'open-xrextension-wrapper :bind "_set_projection_layer_and_get_next_pointer"
  :hash 3744713108 :virtual common-lisp:t)
 int (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-view-locate-info-and-get-next-pointer :class
  'open-xrextension-wrapper :bind "_set_view_locate_info_and_get_next_pointer"
  :hash 3744713108 :virtual common-lisp:t)
 int (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-reference-space-create-info-and-get-next-pointer
  :class 'open-xrextension-wrapper :bind
  "_set_reference_space_create_info_and_get_next_pointer" :hash 50157827
  :virtual common-lisp:t)
 int (reference-space-type int) (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%prepare-view-configuration :class
  'open-xrextension-wrapper :bind "_prepare_view_configuration" :hash
  1286410249 :virtual common-lisp:t)
 :void (view-count int))

(defgmethod
 (open-xrextension-wrapper+%set-view-configuration-and-get-next-pointer :class
  'open-xrextension-wrapper :bind
  "_set_view_configuration_and_get_next_pointer" :hash 50157827 :virtual
  common-lisp:t)
 int (view int) (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%print-view-configuration-info :class
  'open-xrextension-wrapper :bind "_print_view_configuration_info" :hash
  998575451 :virtual common-lisp:t)
 :void (view int))

(defgmethod
 (open-xrextension-wrapper+%get-composition-layer-count :class
  'open-xrextension-wrapper :bind "_get_composition_layer_count" :hash
  2455072627 :virtual common-lisp:t)
 int)

(defgmethod
 (open-xrextension-wrapper+%get-composition-layer :class
  'open-xrextension-wrapper :bind "_get_composition_layer" :hash 3744713108
  :virtual common-lisp:t)
 int (index int))

(defgmethod
 (open-xrextension-wrapper+%get-composition-layer-order :class
  'open-xrextension-wrapper :bind "_get_composition_layer_order" :hash
  3744713108 :virtual common-lisp:t)
 int (index int))

(defgmethod
 (open-xrextension-wrapper+%get-suggested-tracker-names :class
  'open-xrextension-wrapper :bind "_get_suggested_tracker_names" :hash
  2981934095 :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (open-xrextension-wrapper+%on-register-metadata :class
  'open-xrextension-wrapper :bind "_on_register_metadata" :hash 309044627
  :virtual common-lisp:t)
 :void (interaction-profile-metadata open-xrinteraction-profile-metadata))

(defgmethod
 (open-xrextension-wrapper+%on-before-instance-created :class
  'open-xrextension-wrapper :bind "_on_before_instance_created" :hash
  3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-instance-created :class
  'open-xrextension-wrapper :bind "_on_instance_created" :hash 1286410249
  :virtual common-lisp:t)
 :void (instance int))

(defgmethod
 (open-xrextension-wrapper+%on-instance-destroyed :class
  'open-xrextension-wrapper :bind "_on_instance_destroyed" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-session-created :class 'open-xrextension-wrapper
  :bind "_on_session_created" :hash 1286410249 :virtual common-lisp:t)
 :void (session int))

(defgmethod
 (open-xrextension-wrapper+%on-process :class 'open-xrextension-wrapper :bind
  "_on_process" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-sync-actions :class 'open-xrextension-wrapper
  :bind "_on_sync_actions" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-pre-render :class 'open-xrextension-wrapper
  :bind "_on_pre_render" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-main-swapchains-created :class
  'open-xrextension-wrapper :bind "_on_main_swapchains_created" :hash
  3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-pre-draw-viewport :class
  'open-xrextension-wrapper :bind "_on_pre_draw_viewport" :hash 2722037293
  :virtual common-lisp:t)
 :void (viewport rid))

(defgmethod
 (open-xrextension-wrapper+%on-post-draw-viewport :class
  'open-xrextension-wrapper :bind "_on_post_draw_viewport" :hash 2722037293
  :virtual common-lisp:t)
 :void (viewport rid))

(defgmethod
 (open-xrextension-wrapper+%on-session-destroyed :class
  'open-xrextension-wrapper :bind "_on_session_destroyed" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-state-idle :class 'open-xrextension-wrapper
  :bind "_on_state_idle" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-state-ready :class 'open-xrextension-wrapper
  :bind "_on_state_ready" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-state-synchronized :class
  'open-xrextension-wrapper :bind "_on_state_synchronized" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-state-visible :class 'open-xrextension-wrapper
  :bind "_on_state_visible" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-state-focused :class 'open-xrextension-wrapper
  :bind "_on_state_focused" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-state-stopping :class 'open-xrextension-wrapper
  :bind "_on_state_stopping" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-state-loss-pending :class
  'open-xrextension-wrapper :bind "_on_state_loss_pending" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-state-exiting :class 'open-xrextension-wrapper
  :bind "_on_state_exiting" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (open-xrextension-wrapper+%on-event-polled :class 'open-xrextension-wrapper
  :bind "_on_event_polled" :hash 3067735520 :virtual common-lisp:t)
 bool (event (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-viewport-composition-layer-and-get-next-pointer
  :class 'open-xrextension-wrapper :bind
  "_set_viewport_composition_layer_and_get_next_pointer" :hash 2250464348
  :virtual common-lisp:t)
 int (layer (:pointer :void)) (property-values dictionary)
 (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%get-viewport-composition-layer-extension-properties
  :class 'open-xrextension-wrapper :bind
  "_get_viewport_composition_layer_extension_properties" :hash 2915620761
  :virtual common-lisp:t)
 array)

(defgmethod
 (open-xrextension-wrapper+%get-viewport-composition-layer-extension-property-defaults
  :class 'open-xrextension-wrapper :bind
  "_get_viewport_composition_layer_extension_property_defaults" :hash
  2382534195 :virtual common-lisp:t)
 dictionary)

(defgmethod
 (open-xrextension-wrapper+%on-viewport-composition-layer-destroyed :class
  'open-xrextension-wrapper :bind "_on_viewport_composition_layer_destroyed"
  :hash 1286410249 :virtual common-lisp:t)
 :void (layer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+%set-android-surface-swapchain-create-info-and-get-next-pointer
  :class 'open-xrextension-wrapper :bind
  "_set_android_surface_swapchain_create_info_and_get_next_pointer" :hash
  3726637545 :virtual common-lisp:t)
 int (property-values dictionary) (next-pointer (:pointer :void)))

(defgmethod
 (open-xrextension-wrapper+get-openxr-api :class 'open-xrextension-wrapper
  :bind "get_openxr_api" :hash 1637791613)
 open-xrapiextension)

(defgmethod
 (open-xrextension-wrapper+register-extension-wrapper :class
  'open-xrextension-wrapper :bind "register_extension_wrapper" :hash
  3218959716)
 :void)