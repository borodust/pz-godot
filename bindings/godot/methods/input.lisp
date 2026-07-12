(common-lisp:in-package :%godot)


(defgmethod
 (input+is-anything-pressed :class 'input :bind "is_anything_pressed" :hash
  36873697)
 bool)

(defgmethod
 (input+is-key-pressed :class 'input :bind "is_key_pressed" :hash 1938909964)
 bool (keycode key))

(defgmethod
 (input+is-physical-key-pressed :class 'input :bind "is_physical_key_pressed"
  :hash 1938909964)
 bool (keycode key))

(defgmethod
 (input+is-key-label-pressed :class 'input :bind "is_key_label_pressed" :hash
  1938909964)
 bool (keycode key))

(defgmethod
 (input+is-mouse-button-pressed :class 'input :bind "is_mouse_button_pressed"
  :hash 1821097125)
 bool (button mouse-button))

(defgmethod
 (input+is-joy-button-pressed :class 'input :bind "is_joy_button_pressed" :hash
  787208542)
 bool (device int) (button joy-button))

(defgmethod
 (input+is-action-pressed :class 'input :bind "is_action_pressed" :hash
  1558498928)
 bool (action string-name) (exact-match bool))

(defgmethod
 (input+is-action-just-pressed :class 'input :bind "is_action_just_pressed"
  :hash 1558498928)
 bool (action string-name) (exact-match bool))

(defgmethod
 (input+is-action-just-released :class 'input :bind "is_action_just_released"
  :hash 1558498928)
 bool (action string-name) (exact-match bool))

(defgmethod
 (input+is-action-just-pressed-by-event :class 'input :bind
  "is_action_just_pressed_by_event" :hash 551972873)
 bool (action string-name) (event input-event) (exact-match bool))

(defgmethod
 (input+is-action-just-released-by-event :class 'input :bind
  "is_action_just_released_by_event" :hash 551972873)
 bool (action string-name) (event input-event) (exact-match bool))

(defgmethod
 (input+get-action-strength :class 'input :bind "get_action_strength" :hash
  801543509)
 float (action string-name) (exact-match bool))

(defgmethod
 (input+get-action-raw-strength :class 'input :bind "get_action_raw_strength"
  :hash 801543509)
 float (action string-name) (exact-match bool))

(defgmethod (input+get-axis :class 'input :bind "get_axis" :hash 1958752504)
 float (negative-action string-name) (positive-action string-name))

(defgmethod
 (input+get-vector :class 'input :bind "get_vector" :hash 2479607902) vector-2
 (negative-x string-name) (positive-x string-name) (negative-y string-name)
 (positive-y string-name) (deadzone float))

(defgmethod
 (input+add-joy-mapping :class 'input :bind "add_joy_mapping" :hash 1168363258)
 :void (mapping string) (update-existing bool))

(defgmethod
 (input+remove-joy-mapping :class 'input :bind "remove_joy_mapping" :hash
  83702148)
 :void (guid string))

(defgmethod
 (input+is-joy-known :class 'input :bind "is_joy_known" :hash 3067735520) bool
 (device int))

(defgmethod
 (input+get-joy-axis :class 'input :bind "get_joy_axis" :hash 4063175957) float
 (device int) (axis joy-axis))

(defgmethod
 (input+get-joy-name :class 'input :bind "get_joy_name" :hash 990163283) string
 (device int))

(defgmethod
 (input+get-joy-guid :class 'input :bind "get_joy_guid" :hash 844755477) string
 (device int))

(defgmethod
 (input+get-joy-info :class 'input :bind "get_joy_info" :hash 3485342025)
 dictionary (device int))

(defgmethod
 (input+should-ignore-device :class 'input :bind "should_ignore_device" :hash
  2522259332)
 bool (vendor-id int) (product-id int))

(defgmethod
 (input+get-connected-joypads :class 'input :bind "get_connected_joypads" :hash
  2915620761)
 array)

(defgmethod
 (input+get-joy-vibration-strength :class 'input :bind
  "get_joy_vibration_strength" :hash 3114997196)
 vector-2 (device int))

(defgmethod
 (input+get-joy-vibration-duration :class 'input :bind
  "get_joy_vibration_duration" :hash 4025615559)
 float (device int))

(defgmethod
 (input+get-joy-vibration-remaining-duration :class 'input :bind
  "get_joy_vibration_remaining_duration" :hash 4025615559)
 float (device int))

(defgmethod
 (input+is-joy-vibrating :class 'input :bind "is_joy_vibrating" :hash
  3067735520)
 bool (device int))

(defgmethod
 (input+has-joy-vibration :class 'input :bind "has_joy_vibration" :hash
  1116898809)
 bool (device int))

(defgmethod
 (input+start-joy-vibration :class 'input :bind "start_joy_vibration" :hash
  2576575033)
 :void (device int) (weak-magnitude float) (strong-magnitude float)
 (duration float))

(defgmethod
 (input+stop-joy-vibration :class 'input :bind "stop_joy_vibration" :hash
  1286410249)
 :void (device int))

(defgmethod
 (input+vibrate-handheld :class 'input :bind "vibrate_handheld" :hash
  544894297)
 :void (duration-ms int) (amplitude float))

(defgmethod
 (input+set-ignore-joypad-on-unfocused-application :class 'input :bind
  "set_ignore_joypad_on_unfocused_application" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (input+is-ignoring-joypad-on-unfocused-application :class 'input :bind
  "is_ignoring_joypad_on_unfocused_application" :hash 36873697)
 bool)

(defgmethod
 (input+get-gravity :class 'input :bind "get_gravity" :hash 3360562783)
 vector-3)

(defgmethod
 (input+get-accelerometer :class 'input :bind "get_accelerometer" :hash
  3360562783)
 vector-3)

(defgmethod
 (input+get-magnetometer :class 'input :bind "get_magnetometer" :hash
  3360562783)
 vector-3)

(defgmethod
 (input+get-gyroscope :class 'input :bind "get_gyroscope" :hash 3360562783)
 vector-3)

(defgmethod
 (input+get-joy-accelerometer :class 'input :bind "get_joy_accelerometer" :hash
  711720468)
 vector-3 (device int))

(defgmethod
 (input+get-joy-gravity :class 'input :bind "get_joy_gravity" :hash 711720468)
 vector-3 (device int))

(defgmethod
 (input+get-joy-gyroscope :class 'input :bind "get_joy_gyroscope" :hash
  711720468)
 vector-3 (device int))

(defgmethod
 (input+get-joy-motion-sensors-rate :class 'input :bind
  "get_joy_motion_sensors_rate" :hash 2339986948)
 float (device int))

(defgmethod
 (input+is-joy-motion-sensors-enabled :class 'input :bind
  "is_joy_motion_sensors_enabled" :hash 1116898809)
 bool (device int))

(defgmethod
 (input+set-joy-motion-sensors-enabled :class 'input :bind
  "set_joy_motion_sensors_enabled" :hash 300928843)
 :void (device int) (enable bool))

(defgmethod
 (input+has-joy-motion-sensors :class 'input :bind "has_joy_motion_sensors"
  :hash 1116898809)
 bool (device int))

(defgmethod
 (input+start-joy-motion-sensors-calibration :class 'input :bind
  "start_joy_motion_sensors_calibration" :hash 1286410249)
 :void (device int))

(defgmethod
 (input+stop-joy-motion-sensors-calibration :class 'input :bind
  "stop_joy_motion_sensors_calibration" :hash 1286410249)
 :void (device int))

(defgmethod
 (input+clear-joy-motion-sensors-calibration :class 'input :bind
  "clear_joy_motion_sensors_calibration" :hash 1286410249)
 :void (device int))

(defgmethod
 (input+get-joy-motion-sensors-calibration :class 'input :bind
  "get_joy_motion_sensors_calibration" :hash 3485342025)
 dictionary (device int))

(defgmethod
 (input+set-joy-motion-sensors-calibration :class 'input :bind
  "set_joy_motion_sensors_calibration" :hash 64545446)
 :void (device int) (calibration-info dictionary))

(defgmethod
 (input+is-joy-motion-sensors-calibrated :class 'input :bind
  "is_joy_motion_sensors_calibrated" :hash 1116898809)
 bool (device int))

(defgmethod
 (input+is-joy-motion-sensors-calibrating :class 'input :bind
  "is_joy_motion_sensors_calibrating" :hash 1116898809)
 bool (device int))

(defgmethod
 (input+set-gravity :class 'input :bind "set_gravity" :hash 3460891852) :void
 (value vector-3))

(defgmethod
 (input+set-accelerometer :class 'input :bind "set_accelerometer" :hash
  3460891852)
 :void (value vector-3))

(defgmethod
 (input+set-magnetometer :class 'input :bind "set_magnetometer" :hash
  3460891852)
 :void (value vector-3))

(defgmethod
 (input+set-gyroscope :class 'input :bind "set_gyroscope" :hash 3460891852)
 :void (value vector-3))

(defgmethod
 (input+set-joy-light :class 'input :bind "set_joy_light" :hash 2878471219)
 :void (device int) (color color))

(defgmethod
 (input+has-joy-light :class 'input :bind "has_joy_light" :hash 1116898809)
 bool (device int))

(defgmethod
 (input+get-last-mouse-velocity :class 'input :bind "get_last_mouse_velocity"
  :hash 1497962370)
 vector-2)

(defgmethod
 (input+get-last-mouse-screen-velocity :class 'input :bind
  "get_last_mouse_screen_velocity" :hash 1497962370)
 vector-2)

(defgmethod
 (input+get-mouse-button-mask :class 'input :bind "get_mouse_button_mask" :hash
  2512161324)
 mouse-button-mask)

(defgmethod
 (input+set-mouse-mode :class 'input :bind "set_mouse_mode" :hash 2228490894)
 :void (mode input+mouse-mode))

(defgmethod
 (input+get-mouse-mode :class 'input :bind "get_mouse_mode" :hash 965286182)
 input+mouse-mode)

(defgmethod (input+warp-mouse :class 'input :bind "warp_mouse" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (input+action-press :class 'input :bind "action_press" :hash 1713091165) :void
 (action string-name) (strength float))

(defgmethod
 (input+action-release :class 'input :bind "action_release" :hash 3304788590)
 :void (action string-name))

(defgmethod
 (input+set-default-cursor-shape :class 'input :bind "set_default_cursor_shape"
  :hash 2124816902)
 :void (shape input+cursor-shape))

(defgmethod
 (input+get-current-cursor-shape :class 'input :bind "get_current_cursor_shape"
  :hash 3455658929)
 input+cursor-shape)

(defgmethod
 (input+set-custom-mouse-cursor :class 'input :bind "set_custom_mouse_cursor"
  :hash 703945977)
 :void (image resource) (shape input+cursor-shape) (hotspot vector-2))

(defgmethod
 (input+parse-input-event :class 'input :bind "parse_input_event" :hash
  3754044979)
 :void (event input-event))

(defgmethod
 (input+set-use-accumulated-input :class 'input :bind
  "set_use_accumulated_input" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (input+is-using-accumulated-input :class 'input :bind
  "is_using_accumulated_input" :hash 2240911060)
 bool)

(defgmethod
 (input+flush-buffered-events :class 'input :bind "flush_buffered_events" :hash
  3218959716)
 :void)

(defgmethod
 (input+set-emulate-mouse-from-touch :class 'input :bind
  "set_emulate_mouse_from_touch" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (input+is-emulating-mouse-from-touch :class 'input :bind
  "is_emulating_mouse_from_touch" :hash 36873697)
 bool)

(defgmethod
 (input+set-emulate-touch-from-mouse :class 'input :bind
  "set_emulate_touch_from_mouse" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (input+is-emulating-touch-from-mouse :class 'input :bind
  "is_emulating_touch_from_mouse" :hash 36873697)
 bool)