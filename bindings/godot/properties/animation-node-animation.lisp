(common-lisp:in-package :%godot)


(defgproperty animation-node-animation+animation 'animation-node-animation :get
 'animation-node-animation+get-animation :set
 'animation-node-animation+set-animation)

(defgproperty animation-node-animation+play-mode 'animation-node-animation :get
 'animation-node-animation+get-play-mode :set
 'animation-node-animation+set-play-mode)

(defgproperty animation-node-animation+advance-on-start
 'animation-node-animation :get 'animation-node-animation+is-advance-on-start
 :set 'animation-node-animation+set-advance-on-start)

(defgproperty animation-node-animation+use-custom-timeline
 'animation-node-animation :get
 'animation-node-animation+is-using-custom-timeline :set
 'animation-node-animation+set-use-custom-timeline)

(defgproperty animation-node-animation+timeline-length
 'animation-node-animation :get 'animation-node-animation+get-timeline-length
 :set 'animation-node-animation+set-timeline-length)

(defgproperty animation-node-animation+stretch-time-scale
 'animation-node-animation :get
 'animation-node-animation+is-stretching-time-scale :set
 'animation-node-animation+set-stretch-time-scale)

(defgproperty animation-node-animation+start-offset 'animation-node-animation
 :get 'animation-node-animation+get-start-offset :set
 'animation-node-animation+set-start-offset)

(defgproperty animation-node-animation+loop-mode 'animation-node-animation :get
 'animation-node-animation+get-loop-mode :set
 'animation-node-animation+set-loop-mode)