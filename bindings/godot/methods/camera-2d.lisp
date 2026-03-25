(common-lisp:in-package :%godot)


(defgmethod
 (camera-2d+set-offset :class 'camera-2d :bind "set_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (camera-2d+get-offset :class 'camera-2d :bind "get_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (camera-2d+set-anchor-mode :class 'camera-2d :bind "set_anchor_mode" :hash
  2050398218)
 :void (anchor-mode camera-2d+anchor-mode))

(defgmethod
 (camera-2d+get-anchor-mode :class 'camera-2d :bind "get_anchor_mode" :hash
  155978067)
 camera-2d+anchor-mode)

(defgmethod
 (camera-2d+set-ignore-rotation :class 'camera-2d :bind "set_ignore_rotation"
  :hash 2586408642)
 :void (ignore bool))

(defgmethod
 (camera-2d+is-ignoring-rotation :class 'camera-2d :bind "is_ignoring_rotation"
  :hash 36873697)
 bool)

(defgmethod
 (camera-2d+set-process-callback :class 'camera-2d :bind "set_process_callback"
  :hash 4201947462)
 :void (mode camera-2d+camera-2dprocess-callback))

(defgmethod
 (camera-2d+get-process-callback :class 'camera-2d :bind "get_process_callback"
  :hash 2325344499)
 camera-2d+camera-2dprocess-callback)

(defgmethod
 (camera-2d+set-enabled :class 'camera-2d :bind "set_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (camera-2d+is-enabled :class 'camera-2d :bind "is_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-2d+make-current :class 'camera-2d :bind "make_current" :hash
  3218959716)
 :void)

(defgmethod
 (camera-2d+is-current :class 'camera-2d :bind "is_current" :hash 36873697)
 bool)

(defgmethod
 (camera-2d+set-limit-enabled :class 'camera-2d :bind "set_limit_enabled" :hash
  2586408642)
 :void (limit-enabled bool))

(defgmethod
 (camera-2d+is-limit-enabled :class 'camera-2d :bind "is_limit_enabled" :hash
  36873697)
 bool)

(defgmethod
 (camera-2d+set-limit :class 'camera-2d :bind "set_limit" :hash 437707142)
 :void (margin side) (limit int))

(defgmethod
 (camera-2d+get-limit :class 'camera-2d :bind "get_limit" :hash 1983885014) int
 (margin side))

(defgmethod
 (camera-2d+set-limit-smoothing-enabled :class 'camera-2d :bind
  "set_limit_smoothing_enabled" :hash 2586408642)
 :void (limit-smoothing-enabled bool))

(defgmethod
 (camera-2d+is-limit-smoothing-enabled :class 'camera-2d :bind
  "is_limit_smoothing_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-2d+set-drag-vertical-enabled :class 'camera-2d :bind
  "set_drag_vertical_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (camera-2d+is-drag-vertical-enabled :class 'camera-2d :bind
  "is_drag_vertical_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-2d+set-drag-horizontal-enabled :class 'camera-2d :bind
  "set_drag_horizontal_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (camera-2d+is-drag-horizontal-enabled :class 'camera-2d :bind
  "is_drag_horizontal_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-2d+set-drag-vertical-offset :class 'camera-2d :bind
  "set_drag_vertical_offset" :hash 373806689)
 :void (offset float))

(defgmethod
 (camera-2d+get-drag-vertical-offset :class 'camera-2d :bind
  "get_drag_vertical_offset" :hash 1740695150)
 float)

(defgmethod
 (camera-2d+set-drag-horizontal-offset :class 'camera-2d :bind
  "set_drag_horizontal_offset" :hash 373806689)
 :void (offset float))

(defgmethod
 (camera-2d+get-drag-horizontal-offset :class 'camera-2d :bind
  "get_drag_horizontal_offset" :hash 1740695150)
 float)

(defgmethod
 (camera-2d+set-drag-margin :class 'camera-2d :bind "set_drag_margin" :hash
  4290182280)
 :void (margin side) (drag-margin float))

(defgmethod
 (camera-2d+get-drag-margin :class 'camera-2d :bind "get_drag_margin" :hash
  2869120046)
 float (margin side))

(defgmethod
 (camera-2d+get-target-position :class 'camera-2d :bind "get_target_position"
  :hash 3341600327)
 vector-2)

(defgmethod
 (camera-2d+get-screen-center-position :class 'camera-2d :bind
  "get_screen_center_position" :hash 3341600327)
 vector-2)

(defgmethod
 (camera-2d+get-screen-rotation :class 'camera-2d :bind "get_screen_rotation"
  :hash 1740695150)
 float)

(defgmethod
 (camera-2d+set-zoom :class 'camera-2d :bind "set_zoom" :hash 743155724) :void
 (zoom vector-2))

(defgmethod
 (camera-2d+get-zoom :class 'camera-2d :bind "get_zoom" :hash 3341600327)
 vector-2)

(defgmethod
 (camera-2d+set-custom-viewport :class 'camera-2d :bind "set_custom_viewport"
  :hash 1078189570)
 :void (viewport node))

(defgmethod
 (camera-2d+get-custom-viewport :class 'camera-2d :bind "get_custom_viewport"
  :hash 3160264692)
 node)

(defgmethod
 (camera-2d+set-position-smoothing-speed :class 'camera-2d :bind
  "set_position_smoothing_speed" :hash 373806689)
 :void (position-smoothing-speed float))

(defgmethod
 (camera-2d+get-position-smoothing-speed :class 'camera-2d :bind
  "get_position_smoothing_speed" :hash 1740695150)
 float)

(defgmethod
 (camera-2d+set-position-smoothing-enabled :class 'camera-2d :bind
  "set_position_smoothing_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (camera-2d+is-position-smoothing-enabled :class 'camera-2d :bind
  "is_position_smoothing_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-2d+set-rotation-smoothing-enabled :class 'camera-2d :bind
  "set_rotation_smoothing_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (camera-2d+is-rotation-smoothing-enabled :class 'camera-2d :bind
  "is_rotation_smoothing_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-2d+set-rotation-smoothing-speed :class 'camera-2d :bind
  "set_rotation_smoothing_speed" :hash 373806689)
 :void (speed float))

(defgmethod
 (camera-2d+get-rotation-smoothing-speed :class 'camera-2d :bind
  "get_rotation_smoothing_speed" :hash 1740695150)
 float)

(defgmethod
 (camera-2d+force-update-scroll :class 'camera-2d :bind "force_update_scroll"
  :hash 3218959716)
 :void)

(defgmethod
 (camera-2d+reset-smoothing :class 'camera-2d :bind "reset_smoothing" :hash
  3218959716)
 :void)

(defgmethod (camera-2d+align :class 'camera-2d :bind "align" :hash 3218959716)
 :void)

(defgmethod
 (camera-2d+set-screen-drawing-enabled :class 'camera-2d :bind
  "set_screen_drawing_enabled" :hash 2586408642)
 :void (screen-drawing-enabled bool))

(defgmethod
 (camera-2d+is-screen-drawing-enabled :class 'camera-2d :bind
  "is_screen_drawing_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-2d+set-limit-drawing-enabled :class 'camera-2d :bind
  "set_limit_drawing_enabled" :hash 2586408642)
 :void (limit-drawing-enabled bool))

(defgmethod
 (camera-2d+is-limit-drawing-enabled :class 'camera-2d :bind
  "is_limit_drawing_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-2d+set-margin-drawing-enabled :class 'camera-2d :bind
  "set_margin_drawing_enabled" :hash 2586408642)
 :void (margin-drawing-enabled bool))

(defgmethod
 (camera-2d+is-margin-drawing-enabled :class 'camera-2d :bind
  "is_margin_drawing_enabled" :hash 36873697)
 bool)