(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-player-2d+set-stream :class 'audio-stream-player-2d :bind
  "set_stream" :hash 2210767741)
 :void (stream audio-stream))

(defgmethod
 (audio-stream-player-2d+get-stream :class 'audio-stream-player-2d :bind
  "get_stream" :hash 160907539)
 audio-stream)

(defgmethod
 (audio-stream-player-2d+set-volume-db :class 'audio-stream-player-2d :bind
  "set_volume_db" :hash 373806689)
 :void (volume-db float))

(defgmethod
 (audio-stream-player-2d+get-volume-db :class 'audio-stream-player-2d :bind
  "get_volume_db" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-2d+set-volume-linear :class 'audio-stream-player-2d :bind
  "set_volume_linear" :hash 373806689)
 :void (volume-linear float))

(defgmethod
 (audio-stream-player-2d+get-volume-linear :class 'audio-stream-player-2d :bind
  "get_volume_linear" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-2d+set-pitch-scale :class 'audio-stream-player-2d :bind
  "set_pitch_scale" :hash 373806689)
 :void (pitch-scale float))

(defgmethod
 (audio-stream-player-2d+get-pitch-scale :class 'audio-stream-player-2d :bind
  "get_pitch_scale" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-2d+play :class 'audio-stream-player-2d :bind "play" :hash
  1958160172)
 :void (from-position float))

(defgmethod
 (audio-stream-player-2d+seek :class 'audio-stream-player-2d :bind "seek" :hash
  373806689)
 :void (to-position float))

(defgmethod
 (audio-stream-player-2d+stop :class 'audio-stream-player-2d :bind "stop" :hash
  3218959716)
 :void)

(defgmethod
 (audio-stream-player-2d+is-playing :class 'audio-stream-player-2d :bind
  "is_playing" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-player-2d+get-playback-position :class 'audio-stream-player-2d
  :bind "get_playback_position" :hash 191475506)
 float)

(defgmethod
 (audio-stream-player-2d+set-bus :class 'audio-stream-player-2d :bind "set_bus"
  :hash 3304788590)
 :void (bus string-name))

(defgmethod
 (audio-stream-player-2d+get-bus :class 'audio-stream-player-2d :bind "get_bus"
  :hash 2002593661)
 string-name)

(defgmethod
 (audio-stream-player-2d+set-autoplay :class 'audio-stream-player-2d :bind
  "set_autoplay" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (audio-stream-player-2d+is-autoplay-enabled :class 'audio-stream-player-2d
  :bind "is_autoplay_enabled" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-player-2d+set-playing :class 'audio-stream-player-2d :bind
  "set_playing" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (audio-stream-player-2d+set-max-distance :class 'audio-stream-player-2d :bind
  "set_max_distance" :hash 373806689)
 :void (pixels float))

(defgmethod
 (audio-stream-player-2d+get-max-distance :class 'audio-stream-player-2d :bind
  "get_max_distance" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-2d+set-attenuation :class 'audio-stream-player-2d :bind
  "set_attenuation" :hash 373806689)
 :void (curve float))

(defgmethod
 (audio-stream-player-2d+get-attenuation :class 'audio-stream-player-2d :bind
  "get_attenuation" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-2d+set-area-mask :class 'audio-stream-player-2d :bind
  "set_area_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (audio-stream-player-2d+get-area-mask :class 'audio-stream-player-2d :bind
  "get_area_mask" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-player-2d+set-stream-paused :class 'audio-stream-player-2d :bind
  "set_stream_paused" :hash 2586408642)
 :void (pause bool))

(defgmethod
 (audio-stream-player-2d+get-stream-paused :class 'audio-stream-player-2d :bind
  "get_stream_paused" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-player-2d+set-max-polyphony :class 'audio-stream-player-2d :bind
  "set_max_polyphony" :hash 1286410249)
 :void (max-polyphony int))

(defgmethod
 (audio-stream-player-2d+get-max-polyphony :class 'audio-stream-player-2d :bind
  "get_max_polyphony" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-player-2d+set-panning-strength :class 'audio-stream-player-2d
  :bind "set_panning_strength" :hash 373806689)
 :void (panning-strength float))

(defgmethod
 (audio-stream-player-2d+get-panning-strength :class 'audio-stream-player-2d
  :bind "get_panning_strength" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-2d+has-stream-playback :class 'audio-stream-player-2d
  :bind "has_stream_playback" :hash 2240911060)
 bool)

(defgmethod
 (audio-stream-player-2d+get-stream-playback :class 'audio-stream-player-2d
  :bind "get_stream_playback" :hash 210135309)
 audio-stream-playback)

(defgmethod
 (audio-stream-player-2d+set-playback-type :class 'audio-stream-player-2d :bind
  "set_playback_type" :hash 725473817)
 :void (playback-type audio-server+playback-type))

(defgmethod
 (audio-stream-player-2d+get-playback-type :class 'audio-stream-player-2d :bind
  "get_playback_type" :hash 4011264623)
 audio-server+playback-type)