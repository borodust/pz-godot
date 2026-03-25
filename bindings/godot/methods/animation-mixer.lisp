(common-lisp:in-package :%godot)


(defgmethod
 (animation-mixer+-post-process-key-value :class 'animation-mixer :bind
  "_post_process_key_value" :hash 2716908335 :virtual common-lisp:t)
 variant (animation animation) (track int) (value variant) (object-id int)
 (object-sub-idx int))

(defgmethod
 (animation-mixer+add-animation-library :class 'animation-mixer :bind
  "add_animation_library" :hash 618909818)
 error (name string-name) (library animation-library))

(defgmethod
 (animation-mixer+remove-animation-library :class 'animation-mixer :bind
  "remove_animation_library" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (animation-mixer+rename-animation-library :class 'animation-mixer :bind
  "rename_animation_library" :hash 3740211285)
 :void (name string-name) (newname string-name))

(defgmethod
 (animation-mixer+has-animation-library :class 'animation-mixer :bind
  "has_animation_library" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (animation-mixer+get-animation-library :class 'animation-mixer :bind
  "get_animation_library" :hash 147342321)
 animation-library (name string-name))

(defgmethod
 (animation-mixer+get-animation-library-list :class 'animation-mixer :bind
  "get_animation_library_list" :hash 3995934104)
 array)

(defgmethod
 (animation-mixer+has-animation :class 'animation-mixer :bind "has_animation"
  :hash 2619796661)
 bool (name string-name))

(defgmethod
 (animation-mixer+get-animation :class 'animation-mixer :bind "get_animation"
  :hash 2933122410)
 animation (name string-name))

(defgmethod
 (animation-mixer+get-animation-list :class 'animation-mixer :bind
  "get_animation_list" :hash 1139954409)
 packed-string-array)

(defgmethod
 (animation-mixer+set-active :class 'animation-mixer :bind "set_active" :hash
  2586408642)
 :void (active bool))

(defgmethod
 (animation-mixer+is-active :class 'animation-mixer :bind "is_active" :hash
  36873697)
 bool)

(defgmethod
 (animation-mixer+set-deterministic :class 'animation-mixer :bind
  "set_deterministic" :hash 2586408642)
 :void (deterministic bool))

(defgmethod
 (animation-mixer+is-deterministic :class 'animation-mixer :bind
  "is_deterministic" :hash 36873697)
 bool)

(defgmethod
 (animation-mixer+set-root-node :class 'animation-mixer :bind "set_root_node"
  :hash 1348162250)
 :void (path node-path))

(defgmethod
 (animation-mixer+get-root-node :class 'animation-mixer :bind "get_root_node"
  :hash 4075236667)
 node-path)

(defgmethod
 (animation-mixer+set-callback-mode-process :class 'animation-mixer :bind
  "set_callback_mode_process" :hash 2153733086)
 :void (mode animation-mixer+animation-callback-mode-process))

(defgmethod
 (animation-mixer+get-callback-mode-process :class 'animation-mixer :bind
  "get_callback_mode_process" :hash 1394468472)
 animation-mixer+animation-callback-mode-process)

(defgmethod
 (animation-mixer+set-callback-mode-method :class 'animation-mixer :bind
  "set_callback_mode_method" :hash 742218271)
 :void (mode animation-mixer+animation-callback-mode-method))

(defgmethod
 (animation-mixer+get-callback-mode-method :class 'animation-mixer :bind
  "get_callback_mode_method" :hash 489449656)
 animation-mixer+animation-callback-mode-method)

(defgmethod
 (animation-mixer+set-callback-mode-discrete :class 'animation-mixer :bind
  "set_callback_mode_discrete" :hash 1998944670)
 :void (mode animation-mixer+animation-callback-mode-discrete))

(defgmethod
 (animation-mixer+get-callback-mode-discrete :class 'animation-mixer :bind
  "get_callback_mode_discrete" :hash 3493168860)
 animation-mixer+animation-callback-mode-discrete)

(defgmethod
 (animation-mixer+set-audio-max-polyphony :class 'animation-mixer :bind
  "set_audio_max_polyphony" :hash 1286410249)
 :void (max-polyphony int))

(defgmethod
 (animation-mixer+get-audio-max-polyphony :class 'animation-mixer :bind
  "get_audio_max_polyphony" :hash 3905245786)
 int)

(defgmethod
 (animation-mixer+set-root-motion-track :class 'animation-mixer :bind
  "set_root_motion_track" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (animation-mixer+get-root-motion-track :class 'animation-mixer :bind
  "get_root_motion_track" :hash 4075236667)
 node-path)

(defgmethod
 (animation-mixer+set-root-motion-local :class 'animation-mixer :bind
  "set_root_motion_local" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (animation-mixer+is-root-motion-local :class 'animation-mixer :bind
  "is_root_motion_local" :hash 36873697)
 bool)

(defgmethod
 (animation-mixer+get-root-motion-position :class 'animation-mixer :bind
  "get_root_motion_position" :hash 3360562783)
 vector-3)

(defgmethod
 (animation-mixer+get-root-motion-rotation :class 'animation-mixer :bind
  "get_root_motion_rotation" :hash 1222331677)
 quaternion)

(defgmethod
 (animation-mixer+get-root-motion-scale :class 'animation-mixer :bind
  "get_root_motion_scale" :hash 3360562783)
 vector-3)

(defgmethod
 (animation-mixer+get-root-motion-position-accumulator :class 'animation-mixer
  :bind "get_root_motion_position_accumulator" :hash 3360562783)
 vector-3)

(defgmethod
 (animation-mixer+get-root-motion-rotation-accumulator :class 'animation-mixer
  :bind "get_root_motion_rotation_accumulator" :hash 1222331677)
 quaternion)

(defgmethod
 (animation-mixer+get-root-motion-scale-accumulator :class 'animation-mixer
  :bind "get_root_motion_scale_accumulator" :hash 3360562783)
 vector-3)

(defgmethod
 (animation-mixer+clear-caches :class 'animation-mixer :bind "clear_caches"
  :hash 3218959716)
 :void)

(defgmethod
 (animation-mixer+advance :class 'animation-mixer :bind "advance" :hash
  373806689)
 :void (delta float))

(defgmethod
 (animation-mixer+capture :class 'animation-mixer :bind "capture" :hash
  1333632127)
 :void (name string-name) (duration float) (trans-type tween+transition-type)
 (ease-type tween+ease-type))

(defgmethod
 (animation-mixer+set-reset-on-save-enabled :class 'animation-mixer :bind
  "set_reset_on_save_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (animation-mixer+is-reset-on-save-enabled :class 'animation-mixer :bind
  "is_reset_on_save_enabled" :hash 36873697)
 bool)

(defgmethod
 (animation-mixer+find-animation :class 'animation-mixer :bind "find_animation"
  :hash 1559484580)
 string-name (animation animation))

(defgmethod
 (animation-mixer+find-animation-library :class 'animation-mixer :bind
  "find_animation_library" :hash 1559484580)
 string-name (animation animation))