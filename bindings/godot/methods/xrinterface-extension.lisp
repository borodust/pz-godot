(common-lisp:in-package :%godot)


(defgmethod
 (xrinterface-extension+%get-name :class 'xrinterface-extension :bind
  "_get_name" :hash 2002593661 :virtual common-lisp:t)
 string-name)

(defgmethod
 (xrinterface-extension+%get-capabilities :class 'xrinterface-extension :bind
  "_get_capabilities" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (xrinterface-extension+%is-initialized :class 'xrinterface-extension :bind
  "_is_initialized" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (xrinterface-extension+%initialize :class 'xrinterface-extension :bind
  "_initialize" :hash 2240911060 :virtual common-lisp:t)
 bool)

(defgmethod
 (xrinterface-extension+%uninitialize :class 'xrinterface-extension :bind
  "_uninitialize" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (xrinterface-extension+%get-system-info :class 'xrinterface-extension :bind
  "_get_system_info" :hash 3102165223 :virtual common-lisp:t)
 dictionary)

(defgmethod
 (xrinterface-extension+%supports-play-area-mode :class 'xrinterface-extension
  :bind "_supports_play_area_mode" :hash 2693703033 :virtual common-lisp:t)
 bool (mode xrinterface+play-area-mode))

(defgmethod
 (xrinterface-extension+%get-play-area-mode :class 'xrinterface-extension :bind
  "_get_play_area_mode" :hash 1615132885 :virtual common-lisp:t)
 xrinterface+play-area-mode)

(defgmethod
 (xrinterface-extension+%set-play-area-mode :class 'xrinterface-extension :bind
  "_set_play_area_mode" :hash 2693703033 :virtual common-lisp:t)
 bool (mode xrinterface+play-area-mode))

(defgmethod
 (xrinterface-extension+%get-play-area :class 'xrinterface-extension :bind
  "_get_play_area" :hash 497664490 :virtual common-lisp:t)
 packed-vector-3array)

(defgmethod
 (xrinterface-extension+%get-render-target-size :class 'xrinterface-extension
  :bind "_get_render_target_size" :hash 1497962370 :virtual common-lisp:t)
 vector-2)

(defgmethod
 (xrinterface-extension+%get-view-count :class 'xrinterface-extension :bind
  "_get_view_count" :hash 2455072627 :virtual common-lisp:t)
 int)

(defgmethod
 (xrinterface-extension+%get-camera-transform :class 'xrinterface-extension
  :bind "_get_camera_transform" :hash 4183770049 :virtual common-lisp:t)
 transform-3d)

(defgmethod
 (xrinterface-extension+%get-transform-for-view :class 'xrinterface-extension
  :bind "_get_transform_for_view" :hash 518934792 :virtual common-lisp:t)
 transform-3d (view int) (cam-transform transform-3d))

(defgmethod
 (xrinterface-extension+%get-projection-for-view :class 'xrinterface-extension
  :bind "_get_projection_for_view" :hash 4067457445 :virtual common-lisp:t)
 packed-float-64array (view int) (aspect float) (z-near float) (z-far float))

(defgmethod
 (xrinterface-extension+%get-vrs-texture :class 'xrinterface-extension :bind
  "_get_vrs_texture" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (xrinterface-extension+%get-vrs-texture-format :class 'xrinterface-extension
  :bind "_get_vrs_texture_format" :hash 1500923256 :virtual common-lisp:t)
 xrinterface+vrstexture-format)

(defgmethod
 (xrinterface-extension+%process :class 'xrinterface-extension :bind "_process"
  :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (xrinterface-extension+%pre-render :class 'xrinterface-extension :bind
  "_pre_render" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (xrinterface-extension+%pre-draw-viewport :class 'xrinterface-extension :bind
  "_pre_draw_viewport" :hash 3521089500 :virtual common-lisp:t)
 bool (render-target rid))

(defgmethod
 (xrinterface-extension+%post-draw-viewport :class 'xrinterface-extension :bind
  "_post_draw_viewport" :hash 1378122625 :virtual common-lisp:t)
 :void (render-target rid) (screen-rect rect-2))

(defgmethod
 (xrinterface-extension+%end-frame :class 'xrinterface-extension :bind
  "_end_frame" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (xrinterface-extension+%get-suggested-tracker-names :class
  'xrinterface-extension :bind "_get_suggested_tracker_names" :hash 1139954409
  :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (xrinterface-extension+%get-suggested-pose-names :class 'xrinterface-extension
  :bind "_get_suggested_pose_names" :hash 1761182771 :virtual common-lisp:t)
 packed-string-array (tracker-name string-name))

(defgmethod
 (xrinterface-extension+%get-tracking-status :class 'xrinterface-extension
  :bind "_get_tracking_status" :hash 167423259 :virtual common-lisp:t)
 xrinterface+tracking-status)

(defgmethod
 (xrinterface-extension+%trigger-haptic-pulse :class 'xrinterface-extension
  :bind "_trigger_haptic_pulse" :hash 3752640163 :virtual common-lisp:t)
 :void (action-name string) (tracker-name string-name) (frequency float)
 (amplitude float) (duration-sec float) (delay-sec float))

(defgmethod
 (xrinterface-extension+%get-anchor-detection-is-enabled :class
  'xrinterface-extension :bind "_get_anchor_detection_is_enabled" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (xrinterface-extension+%set-anchor-detection-is-enabled :class
  'xrinterface-extension :bind "_set_anchor_detection_is_enabled" :hash
  2586408642 :virtual common-lisp:t)
 :void (enabled bool))

(defgmethod
 (xrinterface-extension+%get-camera-feed-id :class 'xrinterface-extension :bind
  "_get_camera_feed_id" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (xrinterface-extension+%get-color-texture :class 'xrinterface-extension :bind
  "_get_color_texture" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (xrinterface-extension+%get-depth-texture :class 'xrinterface-extension :bind
  "_get_depth_texture" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (xrinterface-extension+%get-velocity-texture :class 'xrinterface-extension
  :bind "_get_velocity_texture" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (xrinterface-extension+get-color-texture :class 'xrinterface-extension :bind
  "get_color_texture" :hash 529393457)
 rid)

(defgmethod
 (xrinterface-extension+get-depth-texture :class 'xrinterface-extension :bind
  "get_depth_texture" :hash 529393457)
 rid)

(defgmethod
 (xrinterface-extension+get-velocity-texture :class 'xrinterface-extension
  :bind "get_velocity_texture" :hash 529393457)
 rid)

(defgmethod
 (xrinterface-extension+add-blit :class 'xrinterface-extension :bind "add_blit"
  :hash 258596971)
 :void (render-target rid) (src-rect rect-2) (dst-rect rect-2i)
 (use-layer bool) (layer int) (apply-lens-distortion bool)
 (eye-center vector-2) (k1 float) (k2 float) (upscale float)
 (aspect-ratio float))

(defgmethod
 (xrinterface-extension+get-render-target-texture :class 'xrinterface-extension
  :bind "get_render_target_texture" :hash 41030802)
 rid (render-target rid))