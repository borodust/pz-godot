(common-lisp:in-package :%godot)


(defgproperty audio-stream-player+stream 'audio-stream-player :get
 'audio-stream-player+get-stream :set 'audio-stream-player+set-stream)

(defgproperty audio-stream-player+volume-db 'audio-stream-player :get
 'audio-stream-player+get-volume-db :set 'audio-stream-player+set-volume-db)

(defgproperty audio-stream-player+volume-linear 'audio-stream-player :get
 'audio-stream-player+get-volume-linear :set
 'audio-stream-player+set-volume-linear)

(defgproperty audio-stream-player+pitch-scale 'audio-stream-player :get
 'audio-stream-player+get-pitch-scale :set 'audio-stream-player+set-pitch-scale)

(defgproperty audio-stream-player+playing 'audio-stream-player :get
 'audio-stream-player+is-playing :set 'audio-stream-player+set-playing)

(defgproperty audio-stream-player+autoplay 'audio-stream-player :get
 'audio-stream-player+is-autoplay-enabled :set
 'audio-stream-player+set-autoplay)

(defgproperty audio-stream-player+stream-paused 'audio-stream-player :get
 'audio-stream-player+get-stream-paused :set
 'audio-stream-player+set-stream-paused)

(defgproperty audio-stream-player+mix-target 'audio-stream-player :get
 'audio-stream-player+get-mix-target :set 'audio-stream-player+set-mix-target)

(defgproperty audio-stream-player+max-polyphony 'audio-stream-player :get
 'audio-stream-player+get-max-polyphony :set
 'audio-stream-player+set-max-polyphony)

(defgproperty audio-stream-player+bus 'audio-stream-player :get
 'audio-stream-player+get-bus :set 'audio-stream-player+set-bus)

(defgproperty audio-stream-player+playback-type 'audio-stream-player :get
 'audio-stream-player+get-playback-type :set
 'audio-stream-player+set-playback-type)