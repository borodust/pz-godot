(common-lisp:in-package :%godot)


(defgmethod
 (editor-debugger-session+send-message :class 'editor-debugger-session :bind
  "send_message" :hash 85656714)
 :void (message string) (data array))

(defgmethod
 (editor-debugger-session+toggle-profiler :class 'editor-debugger-session :bind
  "toggle_profiler" :hash 1198443697)
 :void (profiler string) (enable bool) (data array))

(defgmethod
 (editor-debugger-session+is-breaked :class 'editor-debugger-session :bind
  "is_breaked" :hash 2240911060)
 bool)

(defgmethod
 (editor-debugger-session+is-debuggable :class 'editor-debugger-session :bind
  "is_debuggable" :hash 2240911060)
 bool)

(defgmethod
 (editor-debugger-session+is-active :class 'editor-debugger-session :bind
  "is_active" :hash 2240911060)
 bool)

(defgmethod
 (editor-debugger-session+add-session-tab :class 'editor-debugger-session :bind
  "add_session_tab" :hash 1496901182)
 :void (control control))

(defgmethod
 (editor-debugger-session+remove-session-tab :class 'editor-debugger-session
  :bind "remove_session_tab" :hash 1496901182)
 :void (control control))

(defgmethod
 (editor-debugger-session+set-breakpoint :class 'editor-debugger-session :bind
  "set_breakpoint" :hash 4108344793)
 :void (path string) (line int) (enabled bool))