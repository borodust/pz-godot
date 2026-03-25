(common-lisp:in-package :%godot)


(defgmethod
 (open-xrinterface+get-session-state :class 'open-xrinterface :bind
  "get_session_state" :hash 896364779)
 open-xrinterface+session-state)

(defgmethod
 (open-xrinterface+get-display-refresh-rate :class 'open-xrinterface :bind
  "get_display_refresh_rate" :hash 1740695150)
 float)

(defgmethod
 (open-xrinterface+set-display-refresh-rate :class 'open-xrinterface :bind
  "set_display_refresh_rate" :hash 373806689)
 :void (refresh-rate float))

(defgmethod
 (open-xrinterface+get-render-target-size-multiplier :class 'open-xrinterface
  :bind "get_render_target_size_multiplier" :hash 1740695150)
 float)

(defgmethod
 (open-xrinterface+set-render-target-size-multiplier :class 'open-xrinterface
  :bind "set_render_target_size_multiplier" :hash 373806689)
 :void (multiplier float))

(defgmethod
 (open-xrinterface+is-foveation-supported :class 'open-xrinterface :bind
  "is_foveation_supported" :hash 36873697)
 bool)

(defgmethod
 (open-xrinterface+get-foveation-level :class 'open-xrinterface :bind
  "get_foveation_level" :hash 3905245786)
 int)

(defgmethod
 (open-xrinterface+set-foveation-level :class 'open-xrinterface :bind
  "set_foveation_level" :hash 1286410249)
 :void (foveation-level int))

(defgmethod
 (open-xrinterface+get-foveation-dynamic :class 'open-xrinterface :bind
  "get_foveation_dynamic" :hash 36873697)
 bool)

(defgmethod
 (open-xrinterface+set-foveation-dynamic :class 'open-xrinterface :bind
  "set_foveation_dynamic" :hash 2586408642)
 :void (foveation-dynamic bool))

(defgmethod
 (open-xrinterface+is-action-set-active :class 'open-xrinterface :bind
  "is_action_set_active" :hash 3927539163)
 bool (name string))

(defgmethod
 (open-xrinterface+set-action-set-active :class 'open-xrinterface :bind
  "set_action_set_active" :hash 2678287736)
 :void (name string) (active bool))

(defgmethod
 (open-xrinterface+get-action-sets :class 'open-xrinterface :bind
  "get_action_sets" :hash 3995934104)
 array)

(defgmethod
 (open-xrinterface+get-available-display-refresh-rates :class 'open-xrinterface
  :bind "get_available_display_refresh_rates" :hash 3995934104)
 array)

(defgmethod
 (open-xrinterface+set-motion-range :class 'open-xrinterface :bind
  "set_motion_range" :hash 855158159)
 :void (hand open-xrinterface+hand)
 (motion-range open-xrinterface+hand-motion-range))

(defgmethod
 (open-xrinterface+get-motion-range :class 'open-xrinterface :bind
  "get_motion_range" :hash 3955838114)
 open-xrinterface+hand-motion-range (hand open-xrinterface+hand))

(defgmethod
 (open-xrinterface+get-hand-tracking-source :class 'open-xrinterface :bind
  "get_hand_tracking_source" :hash 4092421202)
 open-xrinterface+hand-tracked-source (hand open-xrinterface+hand))

(defgmethod
 (open-xrinterface+get-hand-joint-flags :class 'open-xrinterface :bind
  "get_hand_joint_flags" :hash 720567706)
 open-xrinterface+hand-joint-flags (hand open-xrinterface+hand)
 (joint open-xrinterface+hand-joints))

(defgmethod
 (open-xrinterface+get-hand-joint-rotation :class 'open-xrinterface :bind
  "get_hand_joint_rotation" :hash 1974618321)
 quaternion (hand open-xrinterface+hand) (joint open-xrinterface+hand-joints))

(defgmethod
 (open-xrinterface+get-hand-joint-position :class 'open-xrinterface :bind
  "get_hand_joint_position" :hash 3529194242)
 vector-3 (hand open-xrinterface+hand) (joint open-xrinterface+hand-joints))

(defgmethod
 (open-xrinterface+get-hand-joint-radius :class 'open-xrinterface :bind
  "get_hand_joint_radius" :hash 901522724)
 float (hand open-xrinterface+hand) (joint open-xrinterface+hand-joints))

(defgmethod
 (open-xrinterface+get-hand-joint-linear-velocity :class 'open-xrinterface
  :bind "get_hand_joint_linear_velocity" :hash 3529194242)
 vector-3 (hand open-xrinterface+hand) (joint open-xrinterface+hand-joints))

(defgmethod
 (open-xrinterface+get-hand-joint-angular-velocity :class 'open-xrinterface
  :bind "get_hand_joint_angular_velocity" :hash 3529194242)
 vector-3 (hand open-xrinterface+hand) (joint open-xrinterface+hand-joints))

(defgmethod
 (open-xrinterface+is-hand-tracking-supported :class 'open-xrinterface :bind
  "is_hand_tracking_supported" :hash 2240911060)
 bool)

(defgmethod
 (open-xrinterface+is-hand-interaction-supported :class 'open-xrinterface :bind
  "is_hand_interaction_supported" :hash 36873697)
 bool)

(defgmethod
 (open-xrinterface+is-eye-gaze-interaction-supported :class 'open-xrinterface
  :bind "is_eye_gaze_interaction_supported" :hash 2240911060)
 bool)

(defgmethod
 (open-xrinterface+get-vrs-min-radius :class 'open-xrinterface :bind
  "get_vrs_min_radius" :hash 1740695150)
 float)

(defgmethod
 (open-xrinterface+set-vrs-min-radius :class 'open-xrinterface :bind
  "set_vrs_min_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (open-xrinterface+get-vrs-strength :class 'open-xrinterface :bind
  "get_vrs_strength" :hash 1740695150)
 float)

(defgmethod
 (open-xrinterface+set-vrs-strength :class 'open-xrinterface :bind
  "set_vrs_strength" :hash 373806689)
 :void (strength float))

(defgmethod
 (open-xrinterface+set-cpu-level :class 'open-xrinterface :bind "set_cpu_level"
  :hash 2940842095)
 :void (level open-xrinterface+perf-settings-level))

(defgmethod
 (open-xrinterface+set-gpu-level :class 'open-xrinterface :bind "set_gpu_level"
  :hash 2940842095)
 :void (level open-xrinterface+perf-settings-level))