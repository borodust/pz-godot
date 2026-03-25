(common-lisp:in-package :%godot)


(defgmethod
 (parallax-background+set-scroll-offset :class 'parallax-background :bind
  "set_scroll_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (parallax-background+get-scroll-offset :class 'parallax-background :bind
  "get_scroll_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (parallax-background+set-scroll-base-offset :class 'parallax-background :bind
  "set_scroll_base_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (parallax-background+get-scroll-base-offset :class 'parallax-background :bind
  "get_scroll_base_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (parallax-background+set-scroll-base-scale :class 'parallax-background :bind
  "set_scroll_base_scale" :hash 743155724)
 :void (scale vector-2))

(defgmethod
 (parallax-background+get-scroll-base-scale :class 'parallax-background :bind
  "get_scroll_base_scale" :hash 3341600327)
 vector-2)

(defgmethod
 (parallax-background+set-limit-begin :class 'parallax-background :bind
  "set_limit_begin" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (parallax-background+get-limit-begin :class 'parallax-background :bind
  "get_limit_begin" :hash 3341600327)
 vector-2)

(defgmethod
 (parallax-background+set-limit-end :class 'parallax-background :bind
  "set_limit_end" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (parallax-background+get-limit-end :class 'parallax-background :bind
  "get_limit_end" :hash 3341600327)
 vector-2)

(defgmethod
 (parallax-background+set-ignore-camera-zoom :class 'parallax-background :bind
  "set_ignore_camera_zoom" :hash 2586408642)
 :void (ignore bool))

(defgmethod
 (parallax-background+is-ignore-camera-zoom :class 'parallax-background :bind
  "is_ignore_camera_zoom" :hash 2240911060)
 bool)