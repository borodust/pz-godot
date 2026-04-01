(common-lisp:in-package :%godot)


(defgmethod
 (editor-debugger-plugin+%setup-session :class 'editor-debugger-plugin :bind
  "_setup_session" :hash 1286410249 :virtual common-lisp:t)
 :void (session-id int))

(defgmethod
 (editor-debugger-plugin+%has-capture :class 'editor-debugger-plugin :bind
  "_has_capture" :hash 3927539163 :virtual common-lisp:t)
 bool (capture string))

(defgmethod
 (editor-debugger-plugin+%capture :class 'editor-debugger-plugin :bind
  "_capture" :hash 2607901833 :virtual common-lisp:t)
 bool (message string) (data array) (session-id int))

(defgmethod
 (editor-debugger-plugin+%goto-script-line :class 'editor-debugger-plugin :bind
  "_goto_script_line" :hash 1208513123 :virtual common-lisp:t)
 :void (script script) (line int))

(defgmethod
 (editor-debugger-plugin+%breakpoints-cleared-in-tree :class
  'editor-debugger-plugin :bind "_breakpoints_cleared_in_tree" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-debugger-plugin+%breakpoint-set-in-tree :class 'editor-debugger-plugin
  :bind "_breakpoint_set_in_tree" :hash 2338735218 :virtual common-lisp:t)
 :void (script script) (line int) (enabled bool))

(defgmethod
 (editor-debugger-plugin+get-session :class 'editor-debugger-plugin :bind
  "get_session" :hash 3061968499)
 editor-debugger-session (id int))

(defgmethod
 (editor-debugger-plugin+get-sessions :class 'editor-debugger-plugin :bind
  "get_sessions" :hash 2915620761)
 array)