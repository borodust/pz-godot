(common-lisp:in-package :%godot)


(defgmethod
 (script-backtrace+get-language-name :class 'script-backtrace :bind
  "get_language_name" :hash 201670096)
 string)

(defgmethod
 (script-backtrace+is-empty :class 'script-backtrace :bind "is_empty" :hash
  36873697)
 bool)

(defgmethod
 (script-backtrace+get-frame-count :class 'script-backtrace :bind
  "get_frame_count" :hash 3905245786)
 int)

(defgmethod
 (script-backtrace+get-frame-function :class 'script-backtrace :bind
  "get_frame_function" :hash 844755477)
 string (index int))

(defgmethod
 (script-backtrace+get-frame-file :class 'script-backtrace :bind
  "get_frame_file" :hash 844755477)
 string (index int))

(defgmethod
 (script-backtrace+get-frame-line :class 'script-backtrace :bind
  "get_frame_line" :hash 923996154)
 int (index int))

(defgmethod
 (script-backtrace+get-global-variable-count :class 'script-backtrace :bind
  "get_global_variable_count" :hash 3905245786)
 int)

(defgmethod
 (script-backtrace+get-global-variable-name :class 'script-backtrace :bind
  "get_global_variable_name" :hash 844755477)
 string (variable-index int))

(defgmethod
 (script-backtrace+get-global-variable-value :class 'script-backtrace :bind
  "get_global_variable_value" :hash 4227898402)
 variant (variable-index int))

(defgmethod
 (script-backtrace+get-local-variable-count :class 'script-backtrace :bind
  "get_local_variable_count" :hash 923996154)
 int (frame-index int))

(defgmethod
 (script-backtrace+get-local-variable-name :class 'script-backtrace :bind
  "get_local_variable_name" :hash 1391810591)
 string (frame-index int) (variable-index int))

(defgmethod
 (script-backtrace+get-local-variable-value :class 'script-backtrace :bind
  "get_local_variable_value" :hash 678354945)
 variant (frame-index int) (variable-index int))

(defgmethod
 (script-backtrace+get-member-variable-count :class 'script-backtrace :bind
  "get_member_variable_count" :hash 923996154)
 int (frame-index int))

(defgmethod
 (script-backtrace+get-member-variable-name :class 'script-backtrace :bind
  "get_member_variable_name" :hash 1391810591)
 string (frame-index int) (variable-index int))

(defgmethod
 (script-backtrace+get-member-variable-value :class 'script-backtrace :bind
  "get_member_variable_value" :hash 678354945)
 variant (frame-index int) (variable-index int))

(defgmethod
 (script-backtrace+format :class 'script-backtrace :bind "format" :hash
  3464456933)
 string (indent-all int) (indent-frames int))