(common-lisp:in-package :%godot)


(defgmethod
 (xrinterface+get-name :class 'xrinterface :bind "get_name" :hash 2002593661)
 string-name)

(defgmethod
 (xrinterface+get-capabilities :class 'xrinterface :bind "get_capabilities"
  :hash 3905245786)
 int)

(defgmethod
 (xrinterface+is-primary :class 'xrinterface :bind "is_primary" :hash
  2240911060)
 bool)

(defgmethod
 (xrinterface+set-primary :class 'xrinterface :bind "set_primary" :hash
  2586408642)
 :void (primary bool))

(defgmethod
 (xrinterface+is-initialized :class 'xrinterface :bind "is_initialized" :hash
  36873697)
 bool)

(defgmethod
 (xrinterface+initialize :class 'xrinterface :bind "initialize" :hash
  2240911060)
 bool)

(defgmethod
 (xrinterface+uninitialize :class 'xrinterface :bind "uninitialize" :hash
  3218959716)
 :void)

(defgmethod
 (xrinterface+get-system-info :class 'xrinterface :bind "get_system_info" :hash
  2382534195)
 dictionary)

(defgmethod
 (xrinterface+get-tracking-status :class 'xrinterface :bind
  "get_tracking_status" :hash 167423259)
 xrinterface+tracking-status)

(defgmethod
 (xrinterface+get-render-target-size :class 'xrinterface :bind
  "get_render_target_size" :hash 1497962370)
 vector-2)

(defgmethod
 (xrinterface+get-view-count :class 'xrinterface :bind "get_view_count" :hash
  2455072627)
 int)

(defgmethod
 (xrinterface+trigger-haptic-pulse :class 'xrinterface :bind
  "trigger_haptic_pulse" :hash 3752640163)
 :void (action-name string) (tracker-name string-name) (frequency float)
 (amplitude float) (duration-sec float) (delay-sec float))

(defgmethod
 (xrinterface+supports-play-area-mode :class 'xrinterface :bind
  "supports_play_area_mode" :hash 3429955281)
 bool (mode xrinterface+play-area-mode))

(defgmethod
 (xrinterface+get-play-area-mode :class 'xrinterface :bind "get_play_area_mode"
  :hash 1615132885)
 xrinterface+play-area-mode)

(defgmethod
 (xrinterface+set-play-area-mode :class 'xrinterface :bind "set_play_area_mode"
  :hash 3429955281)
 bool (mode xrinterface+play-area-mode))

(defgmethod
 (xrinterface+get-play-area :class 'xrinterface :bind "get_play_area" :hash
  497664490)
 packed-vector-3array)

(defgmethod
 (xrinterface+get-anchor-detection-is-enabled :class 'xrinterface :bind
  "get_anchor_detection_is_enabled" :hash 36873697)
 bool)

(defgmethod
 (xrinterface+set-anchor-detection-is-enabled :class 'xrinterface :bind
  "set_anchor_detection_is_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (xrinterface+get-camera-feed-id :class 'xrinterface :bind "get_camera_feed_id"
  :hash 2455072627)
 int)

(defgmethod
 (xrinterface+is-passthrough-supported :class 'xrinterface :bind
  "is_passthrough_supported" :hash 2240911060)
 bool)

(defgmethod
 (xrinterface+is-passthrough-enabled :class 'xrinterface :bind
  "is_passthrough_enabled" :hash 2240911060)
 bool)

(defgmethod
 (xrinterface+start-passthrough :class 'xrinterface :bind "start_passthrough"
  :hash 2240911060)
 bool)

(defgmethod
 (xrinterface+stop-passthrough :class 'xrinterface :bind "stop_passthrough"
  :hash 3218959716)
 :void)

(defgmethod
 (xrinterface+get-transform-for-view :class 'xrinterface :bind
  "get_transform_for_view" :hash 518934792)
 transform-3d (view int) (cam-transform transform-3d))

(defgmethod
 (xrinterface+get-projection-for-view :class 'xrinterface :bind
  "get_projection_for_view" :hash 3766090294)
 projection (view int) (aspect float) (near float) (far float))

(defgmethod
 (xrinterface+get-supported-environment-blend-modes :class 'xrinterface :bind
  "get_supported_environment_blend_modes" :hash 2915620761)
 array)

(defgmethod
 (xrinterface+set-environment-blend-mode :class 'xrinterface :bind
  "set_environment_blend_mode" :hash 551152418)
 bool (mode xrinterface+environment-blend-mode))

(defgmethod
 (xrinterface+get-environment-blend-mode :class 'xrinterface :bind
  "get_environment_blend_mode" :hash 1984334071)
 xrinterface+environment-blend-mode)