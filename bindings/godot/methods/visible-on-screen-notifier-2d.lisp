(common-lisp:in-package :%godot)


(defgmethod
 (visible-on-screen-notifier-2d+set-rect :class 'visible-on-screen-notifier-2d
  :bind "set_rect" :hash 2046264180)
 :void (rect rect-2))

(defgmethod
 (visible-on-screen-notifier-2d+get-rect :class 'visible-on-screen-notifier-2d
  :bind "get_rect" :hash 1639390495)
 rect-2)

(defgmethod
 (visible-on-screen-notifier-2d+set-show-rect :class
  'visible-on-screen-notifier-2d :bind "set_show_rect" :hash 2586408642)
 :void (show-rect bool))

(defgmethod
 (visible-on-screen-notifier-2d+is-showing-rect :class
  'visible-on-screen-notifier-2d :bind "is_showing_rect" :hash 36873697)
 bool)

(defgmethod
 (visible-on-screen-notifier-2d+is-on-screen :class
  'visible-on-screen-notifier-2d :bind "is_on_screen" :hash 36873697)
 bool)