(common-lisp:in-package :%godot)


(defgmethod
 (parallax-2d+set-scroll-scale :class 'parallax-2d :bind "set_scroll_scale"
  :hash 743155724)
 :void (scale vector-2))

(defgmethod
 (parallax-2d+get-scroll-scale :class 'parallax-2d :bind "get_scroll_scale"
  :hash 3341600327)
 vector-2)

(defgmethod
 (parallax-2d+set-repeat-size :class 'parallax-2d :bind "set_repeat_size" :hash
  743155724)
 :void (repeat-size vector-2))

(defgmethod
 (parallax-2d+get-repeat-size :class 'parallax-2d :bind "get_repeat_size" :hash
  3341600327)
 vector-2)

(defgmethod
 (parallax-2d+set-repeat-times :class 'parallax-2d :bind "set_repeat_times"
  :hash 1286410249)
 :void (repeat-times int))

(defgmethod
 (parallax-2d+get-repeat-times :class 'parallax-2d :bind "get_repeat_times"
  :hash 3905245786)
 int)

(defgmethod
 (parallax-2d+set-autoscroll :class 'parallax-2d :bind "set_autoscroll" :hash
  743155724)
 :void (autoscroll vector-2))

(defgmethod
 (parallax-2d+get-autoscroll :class 'parallax-2d :bind "get_autoscroll" :hash
  3341600327)
 vector-2)

(defgmethod
 (parallax-2d+set-scroll-offset :class 'parallax-2d :bind "set_scroll_offset"
  :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (parallax-2d+get-scroll-offset :class 'parallax-2d :bind "get_scroll_offset"
  :hash 3341600327)
 vector-2)

(defgmethod
 (parallax-2d+set-screen-offset :class 'parallax-2d :bind "set_screen_offset"
  :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (parallax-2d+get-screen-offset :class 'parallax-2d :bind "get_screen_offset"
  :hash 3341600327)
 vector-2)

(defgmethod
 (parallax-2d+set-limit-begin :class 'parallax-2d :bind "set_limit_begin" :hash
  743155724)
 :void (offset vector-2))

(defgmethod
 (parallax-2d+get-limit-begin :class 'parallax-2d :bind "get_limit_begin" :hash
  3341600327)
 vector-2)

(defgmethod
 (parallax-2d+set-limit-end :class 'parallax-2d :bind "set_limit_end" :hash
  743155724)
 :void (offset vector-2))

(defgmethod
 (parallax-2d+get-limit-end :class 'parallax-2d :bind "get_limit_end" :hash
  3341600327)
 vector-2)

(defgmethod
 (parallax-2d+set-follow-viewport :class 'parallax-2d :bind
  "set_follow_viewport" :hash 2586408642)
 :void (follow bool))

(defgmethod
 (parallax-2d+get-follow-viewport :class 'parallax-2d :bind
  "get_follow_viewport" :hash 2240911060)
 bool)

(defgmethod
 (parallax-2d+set-ignore-camera-scroll :class 'parallax-2d :bind
  "set_ignore_camera_scroll" :hash 2586408642)
 :void (ignore bool))

(defgmethod
 (parallax-2d+is-ignore-camera-scroll :class 'parallax-2d :bind
  "is_ignore_camera_scroll" :hash 2240911060)
 bool)