(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-player+set-stream :class 'audio-stream-player :bind "set_stream"
  :hash 2210767741)
 :void (stream audio-stream))

(defgmethod
 (audio-stream-player+get-stream :class 'audio-stream-player :bind "get_stream"
  :hash 160907539)
 audio-stream)

(defgmethod
 (audio-stream-player+set-volume-db :class 'audio-stream-player :bind
  "set_volume_db" :hash 373806689)
 :void (volume-db float))

(defgmethod
 (audio-stream-player+get-volume-db :class 'audio-stream-player :bind
  "get_volume_db" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player+set-volume-linear :class 'audio-stream-player :bind
  "set_volume_linear" :hash 373806689)
 :void (volume-linear float))

(defgmethod
 (audio-stream-player+get-volume-linear :class 'audio-stream-player :bind
  "get_volume_linear" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player+set-pitch-scale :class 'audio-stream-player :bind
  "set_pitch_scale" :hash 373806689)
 :void (pitch-scale float))

(defgmethod
 (audio-stream-player+get-pitch-scale :class 'audio-stream-player :bind
  "get_pitch_scale" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player+play :class 'audio-stream-player :bind "play" :hash
  1958160172)
 :void (from-position float))

(defgmethod
 (audio-stream-player+seek :class 'audio-stream-player :bind "seek" :hash
  373806689)
 :void (to-position float))

(defgmethod
 (audio-stream-player+stop :class 'audio-stream-player :bind "stop" :hash
  3218959716)
 :void)

(defgmethod
 (audio-stream-player+is-playing :class 'audio-stream-player :bind "is_playing"
  :hash 36873697)
 bool)

(defgmethod
 (audio-stream-player+get-playback-position :class 'audio-stream-player :bind
  "get_playback_position" :hash 191475506)
 float)

(defgmethod
 (audio-stream-player+set-bus :class 'audio-stream-player :bind "set_bus" :hash
  3304788590)
 :void (bus string-name))

(defgmethod
 (audio-stream-player+get-bus :class 'audio-stream-player :bind "get_bus" :hash
  2002593661)
 string-name)

(defgmethod
 (audio-stream-player+set-autoplay :class 'audio-stream-player :bind
  "set_autoplay" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (audio-stream-player+is-autoplay-enabled :class 'audio-stream-player :bind
  "is_autoplay_enabled" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-player+set-mix-target :class 'audio-stream-player :bind
  "set_mix_target" :hash 2300306138)
 :void (mix-target audio-stream-player+mix-target))

(defgmethod
 (audio-stream-player+get-mix-target :class 'audio-stream-player :bind
  "get_mix_target" :hash 172807476)
 audio-stream-player+mix-target)

(defgmethod
 (audio-stream-player+set-playing :class 'audio-stream-player :bind
  "set_playing" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (audio-stream-player+set-stream-paused :class 'audio-stream-player :bind
  "set_stream_paused" :hash 2586408642)
 :void (pause bool))

(defgmethod
 (audio-stream-player+get-stream-paused :class 'audio-stream-player :bind
  "get_stream_paused" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-player+set-max-polyphony :class 'audio-stream-player :bind
  "set_max_polyphony" :hash 1286410249)
 :void (max-polyphony int))

(defgmethod
 (audio-stream-player+get-max-polyphony :class 'audio-stream-player :bind
  "get_max_polyphony" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-player+has-stream-playback :class 'audio-stream-player :bind
  "has_stream_playback" :hash 2240911060)
 bool)

(defgmethod
 (audio-stream-player+get-stream-playback :class 'audio-stream-player :bind
  "get_stream_playback" :hash 210135309)
 audio-stream-playback)

(defgmethod
 (audio-stream-player+set-playback-type :class 'audio-stream-player :bind
  "set_playback_type" :hash 725473817)
 :void (playback-type audio-server+playback-type))

(defgmethod
 (audio-stream-player+get-playback-type :class 'audio-stream-player :bind
  "get_playback_type" :hash 4011264623)
 audio-server+playback-type)