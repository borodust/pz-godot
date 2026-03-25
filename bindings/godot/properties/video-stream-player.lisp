(common-lisp:in-package :%godot)


(defgproperty video-stream-player+audio-track 'video-stream-player :get
 'video-stream-player+get-audio-track :set 'video-stream-player+set-audio-track)

(defgproperty video-stream-player+stream 'video-stream-player :get
 'video-stream-player+get-stream :set 'video-stream-player+set-stream)

(defgproperty video-stream-player+volume-db 'video-stream-player :get
 'video-stream-player+get-volume-db :set 'video-stream-player+set-volume-db)

(defgproperty video-stream-player+volume 'video-stream-player :get
 'video-stream-player+get-volume :set 'video-stream-player+set-volume)

(defgproperty video-stream-player+speed-scale 'video-stream-player :get
 'video-stream-player+get-speed-scale :set 'video-stream-player+set-speed-scale)

(defgproperty video-stream-player+autoplay 'video-stream-player :get
 'video-stream-player+has-autoplay :set 'video-stream-player+set-autoplay)

(defgproperty video-stream-player+paused 'video-stream-player :get
 'video-stream-player+is-paused :set 'video-stream-player+set-paused)

(defgproperty video-stream-player+expand 'video-stream-player :get
 'video-stream-player+has-expand :set 'video-stream-player+set-expand)

(defgproperty video-stream-player+loop 'video-stream-player :get
 'video-stream-player+has-loop :set 'video-stream-player+set-loop)

(defgproperty video-stream-player+buffering-msec 'video-stream-player :get
 'video-stream-player+get-buffering-msec :set
 'video-stream-player+set-buffering-msec)

(defgproperty video-stream-player+stream-position 'video-stream-player :get
 'video-stream-player+get-stream-position :set
 'video-stream-player+set-stream-position)

(defgproperty video-stream-player+bus 'video-stream-player :get
 'video-stream-player+get-bus :set 'video-stream-player+set-bus)