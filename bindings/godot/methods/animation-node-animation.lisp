(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-animation+set-animation :class 'animation-node-animation :bind
  "set_animation" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (animation-node-animation+get-animation :class 'animation-node-animation :bind
  "get_animation" :hash 2002593661)
 string-name)

(defgmethod
 (animation-node-animation+set-play-mode :class 'animation-node-animation :bind
  "set_play_mode" :hash 3347718873)
 :void (mode animation-node-animation+play-mode))

(defgmethod
 (animation-node-animation+get-play-mode :class 'animation-node-animation :bind
  "get_play_mode" :hash 2061244637)
 animation-node-animation+play-mode)

(defgmethod
 (animation-node-animation+set-advance-on-start :class
  'animation-node-animation :bind "set_advance_on_start" :hash 2586408642)
 :void (advance-on-start bool))

(defgmethod
 (animation-node-animation+is-advance-on-start :class 'animation-node-animation
  :bind "is_advance_on_start" :hash 36873697)
 bool)

(defgmethod
 (animation-node-animation+set-use-custom-timeline :class
  'animation-node-animation :bind "set_use_custom_timeline" :hash 2586408642)
 :void (use-custom-timeline bool))

(defgmethod
 (animation-node-animation+is-using-custom-timeline :class
  'animation-node-animation :bind "is_using_custom_timeline" :hash 36873697)
 bool)

(defgmethod
 (animation-node-animation+set-timeline-length :class 'animation-node-animation
  :bind "set_timeline_length" :hash 373806689)
 :void (timeline-length float))

(defgmethod
 (animation-node-animation+get-timeline-length :class 'animation-node-animation
  :bind "get_timeline_length" :hash 1740695150)
 float)

(defgmethod
 (animation-node-animation+set-stretch-time-scale :class
  'animation-node-animation :bind "set_stretch_time_scale" :hash 2586408642)
 :void (stretch-time-scale bool))

(defgmethod
 (animation-node-animation+is-stretching-time-scale :class
  'animation-node-animation :bind "is_stretching_time_scale" :hash 36873697)
 bool)

(defgmethod
 (animation-node-animation+set-start-offset :class 'animation-node-animation
  :bind "set_start_offset" :hash 373806689)
 :void (start-offset float))

(defgmethod
 (animation-node-animation+get-start-offset :class 'animation-node-animation
  :bind "get_start_offset" :hash 1740695150)
 float)

(defgmethod
 (animation-node-animation+set-loop-mode :class 'animation-node-animation :bind
  "set_loop_mode" :hash 3155355575)
 :void (loop-mode animation+loop-mode))

(defgmethod
 (animation-node-animation+get-loop-mode :class 'animation-node-animation :bind
  "get_loop_mode" :hash 1988889481)
 animation+loop-mode)