(common-lisp:in-package :%godot)


(defgmethod
 (animation-player+animation-set-next :class 'animation-player :bind
  "animation_set_next" :hash 3740211285)
 :void (animation-from string-name) (animation-to string-name))

(defgmethod
 (animation-player+animation-get-next :class 'animation-player :bind
  "animation_get_next" :hash 1965194235)
 string-name (animation-from string-name))

(defgmethod
 (animation-player+set-blend-time :class 'animation-player :bind
  "set_blend_time" :hash 3231131886)
 :void (animation-from string-name) (animation-to string-name) (sec float))

(defgmethod
 (animation-player+get-blend-time :class 'animation-player :bind
  "get_blend_time" :hash 1958752504)
 float (animation-from string-name) (animation-to string-name))

(defgmethod
 (animation-player+set-default-blend-time :class 'animation-player :bind
  "set_default_blend_time" :hash 373806689)
 :void (sec float))

(defgmethod
 (animation-player+get-default-blend-time :class 'animation-player :bind
  "get_default_blend_time" :hash 1740695150)
 float)

(defgmethod
 (animation-player+set-auto-capture :class 'animation-player :bind
  "set_auto_capture" :hash 2586408642)
 :void (auto-capture bool))

(defgmethod
 (animation-player+is-auto-capture :class 'animation-player :bind
  "is_auto_capture" :hash 36873697)
 bool)

(defgmethod
 (animation-player+set-auto-capture-duration :class 'animation-player :bind
  "set_auto_capture_duration" :hash 373806689)
 :void (auto-capture-duration float))

(defgmethod
 (animation-player+get-auto-capture-duration :class 'animation-player :bind
  "get_auto_capture_duration" :hash 1740695150)
 float)

(defgmethod
 (animation-player+set-auto-capture-transition-type :class 'animation-player
  :bind "set_auto_capture_transition_type" :hash 1058637742)
 :void (auto-capture-transition-type tween+transition-type))

(defgmethod
 (animation-player+get-auto-capture-transition-type :class 'animation-player
  :bind "get_auto_capture_transition_type" :hash 3842314528)
 tween+transition-type)

(defgmethod
 (animation-player+set-auto-capture-ease-type :class 'animation-player :bind
  "set_auto_capture_ease_type" :hash 1208105857)
 :void (auto-capture-ease-type tween+ease-type))

(defgmethod
 (animation-player+get-auto-capture-ease-type :class 'animation-player :bind
  "get_auto_capture_ease_type" :hash 631880200)
 tween+ease-type)

(defgmethod
 (animation-player+play :class 'animation-player :bind "play" :hash 3118260607)
 :void (name string-name) (custom-blend float) (custom-speed float)
 (from-end bool))

(defgmethod
 (animation-player+play-section-with-markers :class 'animation-player :bind
  "play_section_with_markers" :hash 1421431412)
 :void (name string-name) (start-marker string-name) (end-marker string-name)
 (custom-blend float) (custom-speed float) (from-end bool))

(defgmethod
 (animation-player+play-section :class 'animation-player :bind "play_section"
  :hash 284774635)
 :void (name string-name) (start-time float) (end-time float)
 (custom-blend float) (custom-speed float) (from-end bool))

(defgmethod
 (animation-player+play-backwards :class 'animation-player :bind
  "play_backwards" :hash 2787282401)
 :void (name string-name) (custom-blend float))

(defgmethod
 (animation-player+play-section-with-markers-backwards :class 'animation-player
  :bind "play_section_with_markers_backwards" :hash 910195100)
 :void (name string-name) (start-marker string-name) (end-marker string-name)
 (custom-blend float))

(defgmethod
 (animation-player+play-section-backwards :class 'animation-player :bind
  "play_section_backwards" :hash 831955981)
 :void (name string-name) (start-time float) (end-time float)
 (custom-blend float))

(defgmethod
 (animation-player+play-with-capture :class 'animation-player :bind
  "play_with_capture" :hash 1572969103)
 :void (name string-name) (duration float) (custom-blend float)
 (custom-speed float) (from-end bool) (trans-type tween+transition-type)
 (ease-type tween+ease-type))

(defgmethod
 (animation-player+pause :class 'animation-player :bind "pause" :hash
  3218959716)
 :void)

(defgmethod
 (animation-player+stop :class 'animation-player :bind "stop" :hash 107499316)
 :void (keep-state bool))

(defgmethod
 (animation-player+is-playing :class 'animation-player :bind "is_playing" :hash
  36873697)
 bool)

(defgmethod
 (animation-player+is-animation-active :class 'animation-player :bind
  "is_animation_active" :hash 36873697)
 bool)

(defgmethod
 (animation-player+set-current-animation :class 'animation-player :bind
  "set_current_animation" :hash 3304788590)
 :void (animation string-name))

(defgmethod
 (animation-player+get-current-animation :class 'animation-player :bind
  "get_current_animation" :hash 2002593661)
 string-name)

(defgmethod
 (animation-player+set-assigned-animation :class 'animation-player :bind
  "set_assigned_animation" :hash 3304788590)
 :void (animation string-name))

(defgmethod
 (animation-player+get-assigned-animation :class 'animation-player :bind
  "get_assigned_animation" :hash 2002593661)
 string-name)

(defgmethod
 (animation-player+queue :class 'animation-player :bind "queue" :hash
  3304788590)
 :void (name string-name))

(defgmethod
 (animation-player+get-queue :class 'animation-player :bind "get_queue" :hash
  2915620761)
 array)

(defgmethod
 (animation-player+clear-queue :class 'animation-player :bind "clear_queue"
  :hash 3218959716)
 :void)

(defgmethod
 (animation-player+set-speed-scale :class 'animation-player :bind
  "set_speed_scale" :hash 373806689)
 :void (speed float))

(defgmethod
 (animation-player+get-speed-scale :class 'animation-player :bind
  "get_speed_scale" :hash 1740695150)
 float)

(defgmethod
 (animation-player+get-playing-speed :class 'animation-player :bind
  "get_playing_speed" :hash 1740695150)
 float)

(defgmethod
 (animation-player+set-autoplay :class 'animation-player :bind "set_autoplay"
  :hash 3304788590)
 :void (name string-name))

(defgmethod
 (animation-player+get-autoplay :class 'animation-player :bind "get_autoplay"
  :hash 2002593661)
 string-name)

(defgmethod
 (animation-player+set-movie-quit-on-finish-enabled :class 'animation-player
  :bind "set_movie_quit_on_finish_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (animation-player+is-movie-quit-on-finish-enabled :class 'animation-player
  :bind "is_movie_quit_on_finish_enabled" :hash 36873697)
 bool)

(defgmethod
 (animation-player+get-current-animation-position :class 'animation-player
  :bind "get_current_animation_position" :hash 1740695150)
 float)

(defgmethod
 (animation-player+get-current-animation-length :class 'animation-player :bind
  "get_current_animation_length" :hash 1740695150)
 float)

(defgmethod
 (animation-player+set-section-with-markers :class 'animation-player :bind
  "set_section_with_markers" :hash 794792241)
 :void (start-marker string-name) (end-marker string-name))

(defgmethod
 (animation-player+set-section :class 'animation-player :bind "set_section"
  :hash 3749779719)
 :void (start-time float) (end-time float))

(defgmethod
 (animation-player+reset-section :class 'animation-player :bind "reset_section"
  :hash 3218959716)
 :void)

(defgmethod
 (animation-player+get-section-start-time :class 'animation-player :bind
  "get_section_start_time" :hash 1740695150)
 float)

(defgmethod
 (animation-player+get-section-end-time :class 'animation-player :bind
  "get_section_end_time" :hash 1740695150)
 float)

(defgmethod
 (animation-player+has-section :class 'animation-player :bind "has_section"
  :hash 36873697)
 bool)

(defgmethod
 (animation-player+seek :class 'animation-player :bind "seek" :hash 1807872683)
 :void (seconds float) (update bool) (update-only bool))

(defgmethod
 (animation-player+set-process-callback :class 'animation-player :bind
  "set_process_callback" :hash 1663839457)
 :void (mode animation-player+animation-process-callback))

(defgmethod
 (animation-player+get-process-callback :class 'animation-player :bind
  "get_process_callback" :hash 4207496604)
 animation-player+animation-process-callback)

(defgmethod
 (animation-player+set-method-call-mode :class 'animation-player :bind
  "set_method_call_mode" :hash 3413514846)
 :void (mode animation-player+animation-method-call-mode))

(defgmethod
 (animation-player+get-method-call-mode :class 'animation-player :bind
  "get_method_call_mode" :hash 3583380054)
 animation-player+animation-method-call-mode)

(defgmethod
 (animation-player+set-root :class 'animation-player :bind "set_root" :hash
  1348162250)
 :void (path node-path))

(defgmethod
 (animation-player+get-root :class 'animation-player :bind "get_root" :hash
  4075236667)
 node-path)