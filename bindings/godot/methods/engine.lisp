(common-lisp:in-package :%godot)


(defgmethod
 (engine+set-physics-ticks-per-second :class 'engine :bind
  "set_physics_ticks_per_second" :hash 1286410249)
 :void (physics-ticks-per-second int))

(defgmethod
 (engine+get-physics-ticks-per-second :class 'engine :bind
  "get_physics_ticks_per_second" :hash 3905245786)
 int)

(defgmethod
 (engine+set-max-physics-steps-per-frame :class 'engine :bind
  "set_max_physics_steps_per_frame" :hash 1286410249)
 :void (max-physics-steps int))

(defgmethod
 (engine+get-max-physics-steps-per-frame :class 'engine :bind
  "get_max_physics_steps_per_frame" :hash 3905245786)
 int)

(defgmethod
 (engine+set-physics-jitter-fix :class 'engine :bind "set_physics_jitter_fix"
  :hash 373806689)
 :void (physics-jitter-fix float))

(defgmethod
 (engine+get-physics-jitter-fix :class 'engine :bind "get_physics_jitter_fix"
  :hash 1740695150)
 float)

(defgmethod
 (engine+get-physics-interpolation-fraction :class 'engine :bind
  "get_physics_interpolation_fraction" :hash 1740695150)
 float)

(defgmethod
 (engine+set-max-fps :class 'engine :bind "set_max_fps" :hash 1286410249) :void
 (max-fps int))

(defgmethod
 (engine+get-max-fps :class 'engine :bind "get_max_fps" :hash 3905245786) int)

(defgmethod
 (engine+set-time-scale :class 'engine :bind "set_time_scale" :hash 373806689)
 :void (time-scale float))

(defgmethod
 (engine+get-time-scale :class 'engine :bind "get_time_scale" :hash 191475506)
 float)

(defgmethod
 (engine+get-frames-drawn :class 'engine :bind "get_frames_drawn" :hash
  2455072627)
 int)

(defgmethod
 (engine+get-frames-per-second :class 'engine :bind "get_frames_per_second"
  :hash 1740695150)
 float)

(defgmethod
 (engine+get-physics-frames :class 'engine :bind "get_physics_frames" :hash
  3905245786)
 int)

(defgmethod
 (engine+get-process-frames :class 'engine :bind "get_process_frames" :hash
  3905245786)
 int)

(defgmethod
 (engine+get-main-loop :class 'engine :bind "get_main_loop" :hash 1016888095)
 main-loop)

(defgmethod
 (engine+get-version-info :class 'engine :bind "get_version_info" :hash
  3102165223)
 dictionary)

(defgmethod
 (engine+get-author-info :class 'engine :bind "get_author_info" :hash
  3102165223)
 dictionary)

(defgmethod
 (engine+get-copyright-info :class 'engine :bind "get_copyright_info" :hash
  3995934104)
 array)

(defgmethod
 (engine+get-donor-info :class 'engine :bind "get_donor_info" :hash 3102165223)
 dictionary)

(defgmethod
 (engine+get-license-info :class 'engine :bind "get_license_info" :hash
  3102165223)
 dictionary)

(defgmethod
 (engine+get-license-text :class 'engine :bind "get_license_text" :hash
  201670096)
 string)

(defgmethod
 (engine+get-architecture-name :class 'engine :bind "get_architecture_name"
  :hash 201670096)
 string)

(defgmethod
 (engine+is-in-physics-frame :class 'engine :bind "is_in_physics_frame" :hash
  36873697)
 bool)

(defgmethod
 (engine+has-singleton :class 'engine :bind "has_singleton" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (engine+get-singleton :class 'engine :bind "get_singleton" :hash 1371597918)
 object (name string-name))

(defgmethod
 (engine+register-singleton :class 'engine :bind "register_singleton" :hash
  965313290)
 :void (name string-name) (instance object))

(defgmethod
 (engine+unregister-singleton :class 'engine :bind "unregister_singleton" :hash
  3304788590)
 :void (name string-name))

(defgmethod
 (engine+get-singleton-list :class 'engine :bind "get_singleton_list" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (engine+register-script-language :class 'engine :bind
  "register_script_language" :hash 1850254898)
 error (language script-language))

(defgmethod
 (engine+unregister-script-language :class 'engine :bind
  "unregister_script_language" :hash 1850254898)
 error (language script-language))

(defgmethod
 (engine+get-script-language-count :class 'engine :bind
  "get_script_language_count" :hash 2455072627)
 int)

(defgmethod
 (engine+get-script-language :class 'engine :bind "get_script_language" :hash
  2151255799)
 script-language (index int))

(defgmethod
 (engine+capture-script-backtraces :class 'engine :bind
  "capture_script_backtraces" :hash 873284517)
 array (include-variables bool))

(defgmethod
 (engine+is-editor-hint :class 'engine :bind "is_editor_hint" :hash 36873697)
 bool)

(defgmethod
 (engine+is-embedded-in-editor :class 'engine :bind "is_embedded_in_editor"
  :hash 36873697)
 bool)

(defgmethod
 (engine+get-write-movie-path :class 'engine :bind "get_write_movie_path" :hash
  201670096)
 string)

(defgmethod
 (engine+set-print-to-stdout :class 'engine :bind "set_print_to_stdout" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (engine+is-printing-to-stdout :class 'engine :bind "is_printing_to_stdout"
  :hash 36873697)
 bool)

(defgmethod
 (engine+set-print-error-messages :class 'engine :bind
  "set_print_error_messages" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (engine+is-printing-error-messages :class 'engine :bind
  "is_printing_error_messages" :hash 36873697)
 bool)