(common-lisp:in-package :%godot)


(defgmethod
 (audio-server+set-bus-count :class 'audio-server :bind "set_bus_count" :hash
  1286410249)
 :void (amount int))

(defgmethod
 (audio-server+get-bus-count :class 'audio-server :bind "get_bus_count" :hash
  3905245786)
 int)

(defgmethod
 (audio-server+remove-bus :class 'audio-server :bind "remove_bus" :hash
  1286410249)
 :void (index int))

(defgmethod
 (audio-server+add-bus :class 'audio-server :bind "add_bus" :hash 1025054187)
 :void (at-position int))

(defgmethod
 (audio-server+move-bus :class 'audio-server :bind "move_bus" :hash 3937882851)
 :void (index int) (to-index int))

(defgmethod
 (audio-server+set-bus-name :class 'audio-server :bind "set_bus_name" :hash
  501894301)
 :void (bus-idx int) (name string))

(defgmethod
 (audio-server+get-bus-name :class 'audio-server :bind "get_bus_name" :hash
  844755477)
 string (bus-idx int))

(defgmethod
 (audio-server+get-bus-index :class 'audio-server :bind "get_bus_index" :hash
  2458036349)
 int (bus-name string-name))

(defgmethod
 (audio-server+get-bus-channels :class 'audio-server :bind "get_bus_channels"
  :hash 923996154)
 int (bus-idx int))

(defgmethod
 (audio-server+set-bus-volume-db :class 'audio-server :bind "set_bus_volume_db"
  :hash 1602489585)
 :void (bus-idx int) (volume-db float))

(defgmethod
 (audio-server+get-bus-volume-db :class 'audio-server :bind "get_bus_volume_db"
  :hash 2339986948)
 float (bus-idx int))

(defgmethod
 (audio-server+set-bus-volume-linear :class 'audio-server :bind
  "set_bus_volume_linear" :hash 1602489585)
 :void (bus-idx int) (volume-linear float))

(defgmethod
 (audio-server+get-bus-volume-linear :class 'audio-server :bind
  "get_bus_volume_linear" :hash 2339986948)
 float (bus-idx int))

(defgmethod
 (audio-server+set-bus-send :class 'audio-server :bind "set_bus_send" :hash
  3780747571)
 :void (bus-idx int) (send string-name))

(defgmethod
 (audio-server+get-bus-send :class 'audio-server :bind "get_bus_send" :hash
  659327637)
 string-name (bus-idx int))

(defgmethod
 (audio-server+set-bus-solo :class 'audio-server :bind "set_bus_solo" :hash
  300928843)
 :void (bus-idx int) (enable bool))

(defgmethod
 (audio-server+is-bus-solo :class 'audio-server :bind "is_bus_solo" :hash
  1116898809)
 bool (bus-idx int))

(defgmethod
 (audio-server+set-bus-mute :class 'audio-server :bind "set_bus_mute" :hash
  300928843)
 :void (bus-idx int) (enable bool))

(defgmethod
 (audio-server+is-bus-mute :class 'audio-server :bind "is_bus_mute" :hash
  1116898809)
 bool (bus-idx int))

(defgmethod
 (audio-server+set-bus-bypass-effects :class 'audio-server :bind
  "set_bus_bypass_effects" :hash 300928843)
 :void (bus-idx int) (enable bool))

(defgmethod
 (audio-server+is-bus-bypassing-effects :class 'audio-server :bind
  "is_bus_bypassing_effects" :hash 1116898809)
 bool (bus-idx int))

(defgmethod
 (audio-server+add-bus-effect :class 'audio-server :bind "add_bus_effect" :hash
  4068819785)
 :void (bus-idx int) (effect audio-effect) (at-position int))

(defgmethod
 (audio-server+remove-bus-effect :class 'audio-server :bind "remove_bus_effect"
  :hash 3937882851)
 :void (bus-idx int) (effect-idx int))

(defgmethod
 (audio-server+get-bus-effect-count :class 'audio-server :bind
  "get_bus_effect_count" :hash 3744713108)
 int (bus-idx int))

(defgmethod
 (audio-server+get-bus-effect :class 'audio-server :bind "get_bus_effect" :hash
  726064442)
 audio-effect (bus-idx int) (effect-idx int))

(defgmethod
 (audio-server+get-bus-effect-instance :class 'audio-server :bind
  "get_bus_effect_instance" :hash 1829771234)
 audio-effect-instance (bus-idx int) (effect-idx int) (channel int))

(defgmethod
 (audio-server+swap-bus-effects :class 'audio-server :bind "swap_bus_effects"
  :hash 1649997291)
 :void (bus-idx int) (effect-idx int) (by-effect-idx int))

(defgmethod
 (audio-server+set-bus-effect-enabled :class 'audio-server :bind
  "set_bus_effect_enabled" :hash 1383440665)
 :void (bus-idx int) (effect-idx int) (enabled bool))

(defgmethod
 (audio-server+is-bus-effect-enabled :class 'audio-server :bind
  "is_bus_effect_enabled" :hash 2522259332)
 bool (bus-idx int) (effect-idx int))

(defgmethod
 (audio-server+get-bus-peak-volume-left-db :class 'audio-server :bind
  "get_bus_peak_volume_left_db" :hash 3085491603)
 float (bus-idx int) (channel int))

(defgmethod
 (audio-server+get-bus-peak-volume-right-db :class 'audio-server :bind
  "get_bus_peak_volume_right_db" :hash 3085491603)
 float (bus-idx int) (channel int))

(defgmethod
 (audio-server+set-playback-speed-scale :class 'audio-server :bind
  "set_playback_speed_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (audio-server+get-playback-speed-scale :class 'audio-server :bind
  "get_playback_speed_scale" :hash 1740695150)
 float)

(defgmethod
 (audio-server+lock :class 'audio-server :bind "lock" :hash 3218959716) :void)

(defgmethod
 (audio-server+unlock :class 'audio-server :bind "unlock" :hash 3218959716)
 :void)

(defgmethod
 (audio-server+get-speaker-mode :class 'audio-server :bind "get_speaker_mode"
  :hash 2549190337)
 audio-server+speaker-mode)

(defgmethod
 (audio-server+get-mix-rate :class 'audio-server :bind "get_mix_rate" :hash
  1740695150)
 float)

(defgmethod
 (audio-server+get-input-mix-rate :class 'audio-server :bind
  "get_input_mix_rate" :hash 1740695150)
 float)

(defgmethod
 (audio-server+get-driver-name :class 'audio-server :bind "get_driver_name"
  :hash 201670096)
 string)

(defgmethod
 (audio-server+get-output-device-list :class 'audio-server :bind
  "get_output_device_list" :hash 2981934095)
 packed-string-array)

(defgmethod
 (audio-server+get-output-device :class 'audio-server :bind "get_output_device"
  :hash 2841200299)
 string)

(defgmethod
 (audio-server+set-output-device :class 'audio-server :bind "set_output_device"
  :hash 83702148)
 :void (name string))

(defgmethod
 (audio-server+get-time-to-next-mix :class 'audio-server :bind
  "get_time_to_next_mix" :hash 1740695150)
 float)

(defgmethod
 (audio-server+get-time-since-last-mix :class 'audio-server :bind
  "get_time_since_last_mix" :hash 1740695150)
 float)

(defgmethod
 (audio-server+get-output-latency :class 'audio-server :bind
  "get_output_latency" :hash 1740695150)
 float)

(defgmethod
 (audio-server+get-input-device-list :class 'audio-server :bind
  "get_input_device_list" :hash 2981934095)
 packed-string-array)

(defgmethod
 (audio-server+get-input-device :class 'audio-server :bind "get_input_device"
  :hash 2841200299)
 string)

(defgmethod
 (audio-server+set-input-device :class 'audio-server :bind "set_input_device"
  :hash 83702148)
 :void (name string))

(defgmethod
 (audio-server+set-input-device-active :class 'audio-server :bind
  "set_input_device_active" :hash 1413768114)
 error (active bool))

(defgmethod
 (audio-server+get-input-frames-available :class 'audio-server :bind
  "get_input_frames_available" :hash 2455072627)
 int)

(defgmethod
 (audio-server+get-input-buffer-length-frames :class 'audio-server :bind
  "get_input_buffer_length_frames" :hash 2455072627)
 int)

(defgmethod
 (audio-server+get-input-frames :class 'audio-server :bind "get_input_frames"
  :hash 2649534757)
 packed-vector-2array (frames int))

(defgmethod
 (audio-server+set-bus-layout :class 'audio-server :bind "set_bus_layout" :hash
  3319058824)
 :void (bus-layout audio-bus-layout))

(defgmethod
 (audio-server+generate-bus-layout :class 'audio-server :bind
  "generate_bus_layout" :hash 3769973890)
 audio-bus-layout)

(defgmethod
 (audio-server+set-enable-tagging-used-audio-streams :class 'audio-server :bind
  "set_enable_tagging_used_audio_streams" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (audio-server+is-stream-registered-as-sample :class 'audio-server :bind
  "is_stream_registered_as_sample" :hash 500225754)
 bool (stream audio-stream))

(defgmethod
 (audio-server+register-stream-as-sample :class 'audio-server :bind
  "register_stream_as_sample" :hash 2210767741)
 :void (stream audio-stream))