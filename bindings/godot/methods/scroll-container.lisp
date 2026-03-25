(common-lisp:in-package :%godot)


(defgmethod
 (scroll-container+set-h-scroll :class 'scroll-container :bind "set_h_scroll"
  :hash 1286410249)
 :void (value int))

(defgmethod
 (scroll-container+get-h-scroll :class 'scroll-container :bind "get_h_scroll"
  :hash 3905245786)
 int)

(defgmethod
 (scroll-container+set-v-scroll :class 'scroll-container :bind "set_v_scroll"
  :hash 1286410249)
 :void (value int))

(defgmethod
 (scroll-container+get-v-scroll :class 'scroll-container :bind "get_v_scroll"
  :hash 3905245786)
 int)

(defgmethod
 (scroll-container+set-horizontal-custom-step :class 'scroll-container :bind
  "set_horizontal_custom_step" :hash 373806689)
 :void (value float))

(defgmethod
 (scroll-container+get-horizontal-custom-step :class 'scroll-container :bind
  "get_horizontal_custom_step" :hash 1740695150)
 float)

(defgmethod
 (scroll-container+set-vertical-custom-step :class 'scroll-container :bind
  "set_vertical_custom_step" :hash 373806689)
 :void (value float))

(defgmethod
 (scroll-container+get-vertical-custom-step :class 'scroll-container :bind
  "get_vertical_custom_step" :hash 1740695150)
 float)

(defgmethod
 (scroll-container+set-horizontal-scroll-mode :class 'scroll-container :bind
  "set_horizontal_scroll_mode" :hash 2750506364)
 :void (enable scroll-container+scroll-mode))

(defgmethod
 (scroll-container+get-horizontal-scroll-mode :class 'scroll-container :bind
  "get_horizontal_scroll_mode" :hash 3987985145)
 scroll-container+scroll-mode)

(defgmethod
 (scroll-container+set-vertical-scroll-mode :class 'scroll-container :bind
  "set_vertical_scroll_mode" :hash 2750506364)
 :void (enable scroll-container+scroll-mode))

(defgmethod
 (scroll-container+get-vertical-scroll-mode :class 'scroll-container :bind
  "get_vertical_scroll_mode" :hash 3987985145)
 scroll-container+scroll-mode)

(defgmethod
 (scroll-container+set-deadzone :class 'scroll-container :bind "set_deadzone"
  :hash 1286410249)
 :void (deadzone int))

(defgmethod
 (scroll-container+get-deadzone :class 'scroll-container :bind "get_deadzone"
  :hash 3905245786)
 int)

(defgmethod
 (scroll-container+set-scroll-hint-mode :class 'scroll-container :bind
  "set_scroll_hint_mode" :hash 578158943)
 :void (scroll-hint-mode scroll-container+scroll-hint-mode))

(defgmethod
 (scroll-container+get-scroll-hint-mode :class 'scroll-container :bind
  "get_scroll_hint_mode" :hash 246835423)
 scroll-container+scroll-hint-mode)

(defgmethod
 (scroll-container+set-tile-scroll-hint :class 'scroll-container :bind
  "set_tile_scroll_hint" :hash 2586408642)
 :void (tile-scroll-hint bool))

(defgmethod
 (scroll-container+is-scroll-hint-tiled :class 'scroll-container :bind
  "is_scroll_hint_tiled" :hash 2240911060)
 bool)

(defgmethod
 (scroll-container+set-follow-focus :class 'scroll-container :bind
  "set_follow_focus" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (scroll-container+is-following-focus :class 'scroll-container :bind
  "is_following_focus" :hash 36873697)
 bool)

(defgmethod
 (scroll-container+get-h-scroll-bar :class 'scroll-container :bind
  "get_h_scroll_bar" :hash 4004517983)
 hscroll-bar)

(defgmethod
 (scroll-container+get-v-scroll-bar :class 'scroll-container :bind
  "get_v_scroll_bar" :hash 2630340773)
 vscroll-bar)

(defgmethod
 (scroll-container+ensure-control-visible :class 'scroll-container :bind
  "ensure_control_visible" :hash 1496901182)
 :void (control control))

(defgmethod
 (scroll-container+set-draw-focus-border :class 'scroll-container :bind
  "set_draw_focus_border" :hash 2586408642)
 :void (draw bool))

(defgmethod
 (scroll-container+get-draw-focus-border :class 'scroll-container :bind
  "get_draw_focus_border" :hash 2240911060)
 bool)