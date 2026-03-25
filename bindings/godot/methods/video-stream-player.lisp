(common-lisp:in-package :%godot)


(defgmethod
 (video-stream-player+set-stream :class 'video-stream-player :bind "set_stream"
  :hash 2317102564)
 :void (stream video-stream))

(defgmethod
 (video-stream-player+get-stream :class 'video-stream-player :bind "get_stream"
  :hash 438621487)
 video-stream)

(defgmethod
 (video-stream-player+play :class 'video-stream-player :bind "play" :hash
  3218959716)
 :void)

(defgmethod
 (video-stream-player+stop :class 'video-stream-player :bind "stop" :hash
  3218959716)
 :void)

(defgmethod
 (video-stream-player+is-playing :class 'video-stream-player :bind "is_playing"
  :hash 36873697)
 bool)

(defgmethod
 (video-stream-player+set-paused :class 'video-stream-player :bind "set_paused"
  :hash 2586408642)
 :void (paused bool))

(defgmethod
 (video-stream-player+is-paused :class 'video-stream-player :bind "is_paused"
  :hash 36873697)
 bool)

(defgmethod
 (video-stream-player+set-loop :class 'video-stream-player :bind "set_loop"
  :hash 2586408642)
 :void (loop bool))

(defgmethod
 (video-stream-player+has-loop :class 'video-stream-player :bind "has_loop"
  :hash 36873697)
 bool)

(defgmethod
 (video-stream-player+set-volume :class 'video-stream-player :bind "set_volume"
  :hash 373806689)
 :void (volume float))

(defgmethod
 (video-stream-player+get-volume :class 'video-stream-player :bind "get_volume"
  :hash 1740695150)
 float)

(defgmethod
 (video-stream-player+set-volume-db :class 'video-stream-player :bind
  "set_volume_db" :hash 373806689)
 :void (db float))

(defgmethod
 (video-stream-player+get-volume-db :class 'video-stream-player :bind
  "get_volume_db" :hash 1740695150)
 float)

(defgmethod
 (video-stream-player+set-speed-scale :class 'video-stream-player :bind
  "set_speed_scale" :hash 373806689)
 :void (speed-scale float))

(defgmethod
 (video-stream-player+get-speed-scale :class 'video-stream-player :bind
  "get_speed_scale" :hash 1740695150)
 float)

(defgmethod
 (video-stream-player+set-audio-track :class 'video-stream-player :bind
  "set_audio_track" :hash 1286410249)
 :void (track int))

(defgmethod
 (video-stream-player+get-audio-track :class 'video-stream-player :bind
  "get_audio_track" :hash 3905245786)
 int)

(defgmethod
 (video-stream-player+get-stream-name :class 'video-stream-player :bind
  "get_stream_name" :hash 201670096)
 string)

(defgmethod
 (video-stream-player+get-stream-length :class 'video-stream-player :bind
  "get_stream_length" :hash 1740695150)
 float)

(defgmethod
 (video-stream-player+set-stream-position :class 'video-stream-player :bind
  "set_stream_position" :hash 373806689)
 :void (position float))

(defgmethod
 (video-stream-player+get-stream-position :class 'video-stream-player :bind
  "get_stream_position" :hash 1740695150)
 float)

(defgmethod
 (video-stream-player+set-autoplay :class 'video-stream-player :bind
  "set_autoplay" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (video-stream-player+has-autoplay :class 'video-stream-player :bind
  "has_autoplay" :hash 36873697)
 bool)

(defgmethod
 (video-stream-player+set-expand :class 'video-stream-player :bind "set_expand"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (video-stream-player+has-expand :class 'video-stream-player :bind "has_expand"
  :hash 36873697)
 bool)

(defgmethod
 (video-stream-player+set-buffering-msec :class 'video-stream-player :bind
  "set_buffering_msec" :hash 1286410249)
 :void (msec int))

(defgmethod
 (video-stream-player+get-buffering-msec :class 'video-stream-player :bind
  "get_buffering_msec" :hash 3905245786)
 int)

(defgmethod
 (video-stream-player+set-bus :class 'video-stream-player :bind "set_bus" :hash
  3304788590)
 :void (bus string-name))

(defgmethod
 (video-stream-player+get-bus :class 'video-stream-player :bind "get_bus" :hash
  2002593661)
 string-name)

(defgmethod
 (video-stream-player+get-video-texture :class 'video-stream-player :bind
  "get_video_texture" :hash 3635182373)
 texture-2d)