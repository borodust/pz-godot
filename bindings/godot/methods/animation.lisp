(common-lisp:in-package :%godot)


(defgmethod
 (animation+add-track :class 'animation :bind "add_track" :hash 3843682357) int
 (type animation+track-type) (at-position int))

(defgmethod
 (animation+remove-track :class 'animation :bind "remove_track" :hash
  1286410249)
 :void (track-idx int))

(defgmethod
 (animation+get-track-count :class 'animation :bind "get_track_count" :hash
  3905245786)
 int)

(defgmethod
 (animation+track-get-type :class 'animation :bind "track_get_type" :hash
  3445944217)
 animation+track-type (track-idx int))

(defgmethod
 (animation+track-get-path :class 'animation :bind "track_get_path" :hash
  408788394)
 node-path (track-idx int))

(defgmethod
 (animation+track-set-path :class 'animation :bind "track_set_path" :hash
  2761262315)
 :void (track-idx int) (path node-path))

(defgmethod
 (animation+find-track :class 'animation :bind "find_track" :hash 245376003)
 int (path node-path) (type animation+track-type))

(defgmethod
 (animation+track-move-up :class 'animation :bind "track_move_up" :hash
  1286410249)
 :void (track-idx int))

(defgmethod
 (animation+track-move-down :class 'animation :bind "track_move_down" :hash
  1286410249)
 :void (track-idx int))

(defgmethod
 (animation+track-move-to :class 'animation :bind "track_move_to" :hash
  3937882851)
 :void (track-idx int) (to-idx int))

(defgmethod
 (animation+track-swap :class 'animation :bind "track_swap" :hash 3937882851)
 :void (track-idx int) (with-idx int))

(defgmethod
 (animation+track-set-imported :class 'animation :bind "track_set_imported"
  :hash 300928843)
 :void (track-idx int) (imported bool))

(defgmethod
 (animation+track-is-imported :class 'animation :bind "track_is_imported" :hash
  1116898809)
 bool (track-idx int))

(defgmethod
 (animation+track-set-enabled :class 'animation :bind "track_set_enabled" :hash
  300928843)
 :void (track-idx int) (enabled bool))

(defgmethod
 (animation+track-is-enabled :class 'animation :bind "track_is_enabled" :hash
  1116898809)
 bool (track-idx int))

(defgmethod
 (animation+position-track-insert-key :class 'animation :bind
  "position_track_insert_key" :hash 2540608232)
 int (track-idx int) (time float) (position vector-3))

(defgmethod
 (animation+rotation-track-insert-key :class 'animation :bind
  "rotation_track_insert_key" :hash 4165004800)
 int (track-idx int) (time float) (rotation quaternion))

(defgmethod
 (animation+scale-track-insert-key :class 'animation :bind
  "scale_track_insert_key" :hash 2540608232)
 int (track-idx int) (time float) (scale vector-3))

(defgmethod
 (animation+blend-shape-track-insert-key :class 'animation :bind
  "blend_shape_track_insert_key" :hash 1534913637)
 int (track-idx int) (time float) (amount float))

(defgmethod
 (animation+position-track-interpolate :class 'animation :bind
  "position_track_interpolate" :hash 3530011197)
 vector-3 (track-idx int) (time-sec float) (backward bool))

(defgmethod
 (animation+rotation-track-interpolate :class 'animation :bind
  "rotation_track_interpolate" :hash 2915876792)
 quaternion (track-idx int) (time-sec float) (backward bool))

(defgmethod
 (animation+scale-track-interpolate :class 'animation :bind
  "scale_track_interpolate" :hash 3530011197)
 vector-3 (track-idx int) (time-sec float) (backward bool))

(defgmethod
 (animation+blend-shape-track-interpolate :class 'animation :bind
  "blend_shape_track_interpolate" :hash 2482365182)
 float (track-idx int) (time-sec float) (backward bool))

(defgmethod
 (animation+track-insert-key :class 'animation :bind "track_insert_key" :hash
  808952278)
 int (track-idx int) (time float) (key variant) (transition float))

(defgmethod
 (animation+track-remove-key :class 'animation :bind "track_remove_key" :hash
  3937882851)
 :void (track-idx int) (key-idx int))

(defgmethod
 (animation+track-remove-key-at-time :class 'animation :bind
  "track_remove_key_at_time" :hash 1602489585)
 :void (track-idx int) (time float))

(defgmethod
 (animation+track-set-key-value :class 'animation :bind "track_set_key_value"
  :hash 2060538656)
 :void (track-idx int) (key int) (value variant))

(defgmethod
 (animation+track-set-key-transition :class 'animation :bind
  "track_set_key_transition" :hash 3506521499)
 :void (track-idx int) (key-idx int) (transition float))

(defgmethod
 (animation+track-set-key-time :class 'animation :bind "track_set_key_time"
  :hash 3506521499)
 :void (track-idx int) (key-idx int) (time float))

(defgmethod
 (animation+track-get-key-transition :class 'animation :bind
  "track_get_key_transition" :hash 3085491603)
 float (track-idx int) (key-idx int))

(defgmethod
 (animation+track-get-key-count :class 'animation :bind "track_get_key_count"
  :hash 923996154)
 int (track-idx int))

(defgmethod
 (animation+track-get-key-value :class 'animation :bind "track_get_key_value"
  :hash 678354945)
 variant (track-idx int) (key-idx int))

(defgmethod
 (animation+track-get-key-time :class 'animation :bind "track_get_key_time"
  :hash 3085491603)
 float (track-idx int) (key-idx int))

(defgmethod
 (animation+track-find-key :class 'animation :bind "track_find_key" :hash
  4230953007)
 int (track-idx int) (time float) (find-mode animation+find-mode) (limit bool)
 (backward bool))

(defgmethod
 (animation+track-set-interpolation-type :class 'animation :bind
  "track_set_interpolation_type" :hash 4112932513)
 :void (track-idx int) (interpolation animation+interpolation-type))

(defgmethod
 (animation+track-get-interpolation-type :class 'animation :bind
  "track_get_interpolation_type" :hash 1530756894)
 animation+interpolation-type (track-idx int))

(defgmethod
 (animation+track-set-interpolation-loop-wrap :class 'animation :bind
  "track_set_interpolation_loop_wrap" :hash 300928843)
 :void (track-idx int) (interpolation bool))

(defgmethod
 (animation+track-get-interpolation-loop-wrap :class 'animation :bind
  "track_get_interpolation_loop_wrap" :hash 1116898809)
 bool (track-idx int))

(defgmethod
 (animation+track-is-compressed :class 'animation :bind "track_is_compressed"
  :hash 1116898809)
 bool (track-idx int))

(defgmethod
 (animation+value-track-set-update-mode :class 'animation :bind
  "value_track_set_update_mode" :hash 2854058312)
 :void (track-idx int) (mode animation+update-mode))

(defgmethod
 (animation+value-track-get-update-mode :class 'animation :bind
  "value_track_get_update_mode" :hash 1440326473)
 animation+update-mode (track-idx int))

(defgmethod
 (animation+value-track-interpolate :class 'animation :bind
  "value_track_interpolate" :hash 747269075)
 variant (track-idx int) (time-sec float) (backward bool))

(defgmethod
 (animation+method-track-get-name :class 'animation :bind
  "method_track_get_name" :hash 351665558)
 string-name (track-idx int) (key-idx int))

(defgmethod
 (animation+method-track-get-params :class 'animation :bind
  "method_track_get_params" :hash 2345056839)
 array (track-idx int) (key-idx int))

(defgmethod
 (animation+bezier-track-insert-key :class 'animation :bind
  "bezier_track_insert_key" :hash 3656773645)
 int (track-idx int) (time float) (value float) (in-handle vector-2)
 (out-handle vector-2))

(defgmethod
 (animation+bezier-track-set-key-value :class 'animation :bind
  "bezier_track_set_key_value" :hash 3506521499)
 :void (track-idx int) (key-idx int) (value float))

(defgmethod
 (animation+bezier-track-set-key-in-handle :class 'animation :bind
  "bezier_track_set_key_in_handle" :hash 1719223284)
 :void (track-idx int) (key-idx int) (in-handle vector-2)
 (balanced-value-time-ratio float))

(defgmethod
 (animation+bezier-track-set-key-out-handle :class 'animation :bind
  "bezier_track_set_key_out_handle" :hash 1719223284)
 :void (track-idx int) (key-idx int) (out-handle vector-2)
 (balanced-value-time-ratio float))

(defgmethod
 (animation+bezier-track-get-key-value :class 'animation :bind
  "bezier_track_get_key_value" :hash 3085491603)
 float (track-idx int) (key-idx int))

(defgmethod
 (animation+bezier-track-get-key-in-handle :class 'animation :bind
  "bezier_track_get_key_in_handle" :hash 3016396712)
 vector-2 (track-idx int) (key-idx int))

(defgmethod
 (animation+bezier-track-get-key-out-handle :class 'animation :bind
  "bezier_track_get_key_out_handle" :hash 3016396712)
 vector-2 (track-idx int) (key-idx int))

(defgmethod
 (animation+bezier-track-interpolate :class 'animation :bind
  "bezier_track_interpolate" :hash 1900462983)
 float (track-idx int) (time float))

(defgmethod
 (animation+audio-track-insert-key :class 'animation :bind
  "audio_track_insert_key" :hash 4021027286)
 int (track-idx int) (time float) (stream resource) (start-offset float)
 (end-offset float))

(defgmethod
 (animation+audio-track-set-key-stream :class 'animation :bind
  "audio_track_set_key_stream" :hash 3886397084)
 :void (track-idx int) (key-idx int) (stream resource))

(defgmethod
 (animation+audio-track-set-key-start-offset :class 'animation :bind
  "audio_track_set_key_start_offset" :hash 3506521499)
 :void (track-idx int) (key-idx int) (offset float))

(defgmethod
 (animation+audio-track-set-key-end-offset :class 'animation :bind
  "audio_track_set_key_end_offset" :hash 3506521499)
 :void (track-idx int) (key-idx int) (offset float))

(defgmethod
 (animation+audio-track-get-key-stream :class 'animation :bind
  "audio_track_get_key_stream" :hash 635277205)
 resource (track-idx int) (key-idx int))

(defgmethod
 (animation+audio-track-get-key-start-offset :class 'animation :bind
  "audio_track_get_key_start_offset" :hash 3085491603)
 float (track-idx int) (key-idx int))

(defgmethod
 (animation+audio-track-get-key-end-offset :class 'animation :bind
  "audio_track_get_key_end_offset" :hash 3085491603)
 float (track-idx int) (key-idx int))

(defgmethod
 (animation+audio-track-set-use-blend :class 'animation :bind
  "audio_track_set_use_blend" :hash 300928843)
 :void (track-idx int) (enable bool))

(defgmethod
 (animation+audio-track-is-use-blend :class 'animation :bind
  "audio_track_is_use_blend" :hash 1116898809)
 bool (track-idx int))

(defgmethod
 (animation+animation-track-insert-key :class 'animation :bind
  "animation_track_insert_key" :hash 158676774)
 int (track-idx int) (time float) (animation string-name))

(defgmethod
 (animation+animation-track-set-key-animation :class 'animation :bind
  "animation_track_set_key_animation" :hash 117615382)
 :void (track-idx int) (key-idx int) (animation string-name))

(defgmethod
 (animation+animation-track-get-key-animation :class 'animation :bind
  "animation_track_get_key_animation" :hash 351665558)
 string-name (track-idx int) (key-idx int))

(defgmethod
 (animation+add-marker :class 'animation :bind "add_marker" :hash 4135858297)
 :void (name string-name) (time float))

(defgmethod
 (animation+remove-marker :class 'animation :bind "remove_marker" :hash
  3304788590)
 :void (name string-name))

(defgmethod
 (animation+has-marker :class 'animation :bind "has_marker" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (animation+get-marker-at-time :class 'animation :bind "get_marker_at_time"
  :hash 4079494655)
 string-name (time float))

(defgmethod
 (animation+get-next-marker :class 'animation :bind "get_next_marker" :hash
  4079494655)
 string-name (time float))

(defgmethod
 (animation+get-prev-marker :class 'animation :bind "get_prev_marker" :hash
  4079494655)
 string-name (time float))

(defgmethod
 (animation+get-marker-time :class 'animation :bind "get_marker_time" :hash
  2349060816)
 float (name string-name))

(defgmethod
 (animation+get-marker-names :class 'animation :bind "get_marker_names" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (animation+get-marker-color :class 'animation :bind "get_marker_color" :hash
  3742943038)
 color (name string-name))

(defgmethod
 (animation+set-marker-color :class 'animation :bind "set_marker_color" :hash
  4260178595)
 :void (name string-name) (color color))

(defgmethod
 (animation+set-length :class 'animation :bind "set_length" :hash 373806689)
 :void (time-sec float))

(defgmethod
 (animation+get-length :class 'animation :bind "get_length" :hash 1740695150)
 float)

(defgmethod
 (animation+set-loop-mode :class 'animation :bind "set_loop_mode" :hash
  3155355575)
 :void (loop-mode animation+loop-mode))

(defgmethod
 (animation+get-loop-mode :class 'animation :bind "get_loop_mode" :hash
  1988889481)
 animation+loop-mode)

(defgmethod
 (animation+set-step :class 'animation :bind "set_step" :hash 373806689) :void
 (size-sec float))

(defgmethod
 (animation+get-step :class 'animation :bind "get_step" :hash 1740695150) float)

(defgmethod (animation+clear :class 'animation :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (animation+copy-track :class 'animation :bind "copy_track" :hash 148001024)
 :void (track-idx int) (to-animation animation))

(defgmethod
 (animation+optimize :class 'animation :bind "optimize" :hash 3303583852) :void
 (allowed-velocity-err float) (allowed-angular-err float) (precision int))

(defgmethod
 (animation+compress :class 'animation :bind "compress" :hash 3608408117) :void
 (page-size int) (fps int) (split-tolerance float))

(defgmethod
 (animation+is-capture-included :class 'animation :bind "is_capture_included"
  :hash 36873697)
 bool)