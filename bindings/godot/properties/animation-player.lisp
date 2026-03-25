(common-lisp:in-package :%godot)


(defgproperty animation-player+current-animation 'animation-player :get
 'animation-player+get-current-animation :set
 'animation-player+set-current-animation)

(defgproperty animation-player+assigned-animation 'animation-player :get
 'animation-player+get-assigned-animation :set
 'animation-player+set-assigned-animation)

(defgproperty animation-player+autoplay 'animation-player :get
 'animation-player+get-autoplay :set 'animation-player+set-autoplay)

(defgproperty animation-player+current-animation-length 'animation-player :get
 'animation-player+get-current-animation-length)

(defgproperty animation-player+current-animation-position 'animation-player
 :get 'animation-player+get-current-animation-position)

(defgproperty animation-player+playback-auto-capture 'animation-player :get
 'animation-player+is-auto-capture :set 'animation-player+set-auto-capture)

(defgproperty animation-player+playback-auto-capture-duration 'animation-player
 :get 'animation-player+get-auto-capture-duration :set
 'animation-player+set-auto-capture-duration)

(defgproperty animation-player+playback-auto-capture-transition-type
 'animation-player :get 'animation-player+get-auto-capture-transition-type :set
 'animation-player+set-auto-capture-transition-type)

(defgproperty animation-player+playback-auto-capture-ease-type
 'animation-player :get 'animation-player+get-auto-capture-ease-type :set
 'animation-player+set-auto-capture-ease-type)

(defgproperty animation-player+playback-default-blend-time 'animation-player
 :get 'animation-player+get-default-blend-time :set
 'animation-player+set-default-blend-time)

(defgproperty animation-player+speed-scale 'animation-player :get
 'animation-player+get-speed-scale :set 'animation-player+set-speed-scale)

(defgproperty animation-player+movie-quit-on-finish 'animation-player :get
 'animation-player+is-movie-quit-on-finish-enabled :set
 'animation-player+set-movie-quit-on-finish-enabled)