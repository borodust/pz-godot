(common-lisp:in-package :%godot)


(defgmethod
 (engine-debugger+is-active :class 'engine-debugger :bind "is_active" :hash
  2240911060)
 bool)

(defgmethod
 (engine-debugger+register-profiler :class 'engine-debugger :bind
  "register_profiler" :hash 3651669560)
 :void (name string-name) (profiler engine-profiler))

(defgmethod
 (engine-debugger+unregister-profiler :class 'engine-debugger :bind
  "unregister_profiler" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (engine-debugger+is-profiling :class 'engine-debugger :bind "is_profiling"
  :hash 2041966384)
 bool (name string-name))

(defgmethod
 (engine-debugger+has-profiler :class 'engine-debugger :bind "has_profiler"
  :hash 2041966384)
 bool (name string-name))

(defgmethod
 (engine-debugger+profiler-add-frame-data :class 'engine-debugger :bind
  "profiler_add_frame_data" :hash 1895267858)
 :void (name string-name) (data array))

(defgmethod
 (engine-debugger+profiler-enable :class 'engine-debugger :bind
  "profiler_enable" :hash 3192561009)
 :void (name string-name) (enable bool) (arguments array))

(defgmethod
 (engine-debugger+register-message-capture :class 'engine-debugger :bind
  "register_message_capture" :hash 1874754934)
 :void (name string-name) (callable callable))

(defgmethod
 (engine-debugger+unregister-message-capture :class 'engine-debugger :bind
  "unregister_message_capture" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (engine-debugger+has-capture :class 'engine-debugger :bind "has_capture" :hash
  2041966384)
 bool (name string-name))

(defgmethod
 (engine-debugger+line-poll :class 'engine-debugger :bind "line_poll" :hash
  3218959716)
 :void)

(defgmethod
 (engine-debugger+send-message :class 'engine-debugger :bind "send_message"
  :hash 1209351045)
 :void (message string) (data array))

(defgmethod
 (engine-debugger+debug :class 'engine-debugger :bind "debug" :hash 2751962654)
 :void (can-continue bool) (is-error-breakpoint bool))

(defgmethod
 (engine-debugger+script-debug :class 'engine-debugger :bind "script_debug"
  :hash 2442343672)
 :void (language script-language) (can-continue bool)
 (is-error-breakpoint bool))

(defgmethod
 (engine-debugger+set-lines-left :class 'engine-debugger :bind "set_lines_left"
  :hash 1286410249)
 :void (lines int))

(defgmethod
 (engine-debugger+get-lines-left :class 'engine-debugger :bind "get_lines_left"
  :hash 3905245786)
 int)

(defgmethod
 (engine-debugger+set-depth :class 'engine-debugger :bind "set_depth" :hash
  1286410249)
 :void (depth int))

(defgmethod
 (engine-debugger+get-depth :class 'engine-debugger :bind "get_depth" :hash
  3905245786)
 int)

(defgmethod
 (engine-debugger+is-breakpoint :class 'engine-debugger :bind "is_breakpoint"
  :hash 921227809)
 bool (line int) (source string-name))

(defgmethod
 (engine-debugger+is-skipping-breakpoints :class 'engine-debugger :bind
  "is_skipping_breakpoints" :hash 36873697)
 bool)

(defgmethod
 (engine-debugger+insert-breakpoint :class 'engine-debugger :bind
  "insert_breakpoint" :hash 3780747571)
 :void (line int) (source string-name))

(defgmethod
 (engine-debugger+remove-breakpoint :class 'engine-debugger :bind
  "remove_breakpoint" :hash 3780747571)
 :void (line int) (source string-name))

(defgmethod
 (engine-debugger+clear-breakpoints :class 'engine-debugger :bind
  "clear_breakpoints" :hash 3218959716)
 :void)