(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-player-3d+set-stream :class 'audio-stream-player-3d :bind
  "set_stream" :hash 2210767741)
 :void (stream audio-stream))

(defgmethod
 (audio-stream-player-3d+get-stream :class 'audio-stream-player-3d :bind
  "get_stream" :hash 160907539)
 audio-stream)

(defgmethod
 (audio-stream-player-3d+set-volume-db :class 'audio-stream-player-3d :bind
  "set_volume_db" :hash 373806689)
 :void (volume-db float))

(defgmethod
 (audio-stream-player-3d+get-volume-db :class 'audio-stream-player-3d :bind
  "get_volume_db" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+set-volume-linear :class 'audio-stream-player-3d :bind
  "set_volume_linear" :hash 373806689)
 :void (volume-linear float))

(defgmethod
 (audio-stream-player-3d+get-volume-linear :class 'audio-stream-player-3d :bind
  "get_volume_linear" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+set-unit-size :class 'audio-stream-player-3d :bind
  "set_unit_size" :hash 373806689)
 :void (unit-size float))

(defgmethod
 (audio-stream-player-3d+get-unit-size :class 'audio-stream-player-3d :bind
  "get_unit_size" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+set-max-db :class 'audio-stream-player-3d :bind
  "set_max_db" :hash 373806689)
 :void (max-db float))

(defgmethod
 (audio-stream-player-3d+get-max-db :class 'audio-stream-player-3d :bind
  "get_max_db" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+set-pitch-scale :class 'audio-stream-player-3d :bind
  "set_pitch_scale" :hash 373806689)
 :void (pitch-scale float))

(defgmethod
 (audio-stream-player-3d+get-pitch-scale :class 'audio-stream-player-3d :bind
  "get_pitch_scale" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+play :class 'audio-stream-player-3d :bind "play" :hash
  1958160172)
 :void (from-position float))

(defgmethod
 (audio-stream-player-3d+seek :class 'audio-stream-player-3d :bind "seek" :hash
  373806689)
 :void (to-position float))

(defgmethod
 (audio-stream-player-3d+stop :class 'audio-stream-player-3d :bind "stop" :hash
  3218959716)
 :void)

(defgmethod
 (audio-stream-player-3d+is-playing :class 'audio-stream-player-3d :bind
  "is_playing" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-player-3d+get-playback-position :class 'audio-stream-player-3d
  :bind "get_playback_position" :hash 191475506)
 float)

(defgmethod
 (audio-stream-player-3d+set-bus :class 'audio-stream-player-3d :bind "set_bus"
  :hash 3304788590)
 :void (bus string-name))

(defgmethod
 (audio-stream-player-3d+get-bus :class 'audio-stream-player-3d :bind "get_bus"
  :hash 2002593661)
 string-name)

(defgmethod
 (audio-stream-player-3d+set-autoplay :class 'audio-stream-player-3d :bind
  "set_autoplay" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (audio-stream-player-3d+is-autoplay-enabled :class 'audio-stream-player-3d
  :bind "is_autoplay_enabled" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-player-3d+set-playing :class 'audio-stream-player-3d :bind
  "set_playing" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (audio-stream-player-3d+set-max-distance :class 'audio-stream-player-3d :bind
  "set_max_distance" :hash 373806689)
 :void (meters float))

(defgmethod
 (audio-stream-player-3d+get-max-distance :class 'audio-stream-player-3d :bind
  "get_max_distance" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+set-area-mask :class 'audio-stream-player-3d :bind
  "set_area_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (audio-stream-player-3d+get-area-mask :class 'audio-stream-player-3d :bind
  "get_area_mask" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-player-3d+set-emission-angle :class 'audio-stream-player-3d
  :bind "set_emission_angle" :hash 373806689)
 :void (degrees float))

(defgmethod
 (audio-stream-player-3d+get-emission-angle :class 'audio-stream-player-3d
  :bind "get_emission_angle" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+set-emission-angle-enabled :class
  'audio-stream-player-3d :bind "set_emission_angle_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (audio-stream-player-3d+is-emission-angle-enabled :class
  'audio-stream-player-3d :bind "is_emission_angle_enabled" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-player-3d+set-emission-angle-filter-attenuation-db :class
  'audio-stream-player-3d :bind "set_emission_angle_filter_attenuation_db"
  :hash 373806689)
 :void (db float))

(defgmethod
 (audio-stream-player-3d+get-emission-angle-filter-attenuation-db :class
  'audio-stream-player-3d :bind "get_emission_angle_filter_attenuation_db"
  :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+set-attenuation-filter-cutoff-hz :class
  'audio-stream-player-3d :bind "set_attenuation_filter_cutoff_hz" :hash
  373806689)
 :void (degrees float))

(defgmethod
 (audio-stream-player-3d+get-attenuation-filter-cutoff-hz :class
  'audio-stream-player-3d :bind "get_attenuation_filter_cutoff_hz" :hash
  1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+set-attenuation-filter-db :class
  'audio-stream-player-3d :bind "set_attenuation_filter_db" :hash 373806689)
 :void (db float))

(defgmethod
 (audio-stream-player-3d+get-attenuation-filter-db :class
  'audio-stream-player-3d :bind "get_attenuation_filter_db" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+set-attenuation-model :class 'audio-stream-player-3d
  :bind "set_attenuation_model" :hash 2988086229)
 :void (model audio-stream-player-3d+attenuation-model))

(defgmethod
 (audio-stream-player-3d+get-attenuation-model :class 'audio-stream-player-3d
  :bind "get_attenuation_model" :hash 3035106060)
 audio-stream-player-3d+attenuation-model)

(defgmethod
 (audio-stream-player-3d+set-doppler-tracking :class 'audio-stream-player-3d
  :bind "set_doppler_tracking" :hash 3968161450)
 :void (mode audio-stream-player-3d+doppler-tracking))

(defgmethod
 (audio-stream-player-3d+get-doppler-tracking :class 'audio-stream-player-3d
  :bind "get_doppler_tracking" :hash 1702418664)
 audio-stream-player-3d+doppler-tracking)

(defgmethod
 (audio-stream-player-3d+set-stream-paused :class 'audio-stream-player-3d :bind
  "set_stream_paused" :hash 2586408642)
 :void (pause bool))

(defgmethod
 (audio-stream-player-3d+get-stream-paused :class 'audio-stream-player-3d :bind
  "get_stream_paused" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-player-3d+set-max-polyphony :class 'audio-stream-player-3d :bind
  "set_max_polyphony" :hash 1286410249)
 :void (max-polyphony int))

(defgmethod
 (audio-stream-player-3d+get-max-polyphony :class 'audio-stream-player-3d :bind
  "get_max_polyphony" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-player-3d+set-panning-strength :class 'audio-stream-player-3d
  :bind "set_panning_strength" :hash 373806689)
 :void (panning-strength float))

(defgmethod
 (audio-stream-player-3d+get-panning-strength :class 'audio-stream-player-3d
  :bind "get_panning_strength" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-player-3d+has-stream-playback :class 'audio-stream-player-3d
  :bind "has_stream_playback" :hash 2240911060)
 bool)

(defgmethod
 (audio-stream-player-3d+get-stream-playback :class 'audio-stream-player-3d
  :bind "get_stream_playback" :hash 210135309)
 audio-stream-playback)

(defgmethod
 (audio-stream-player-3d+set-playback-type :class 'audio-stream-player-3d :bind
  "set_playback_type" :hash 725473817)
 :void (playback-type audio-server+playback-type))

(defgmethod
 (audio-stream-player-3d+get-playback-type :class 'audio-stream-player-3d :bind
  "get_playback_type" :hash 4011264623)
 audio-server+playback-type)