(common-lisp:in-package :%godot)


(defgmethod
 (logger+-log-error :class 'logger :bind "_log_error" :hash 27079556 :virtual
  common-lisp:t)
 :void (function string) (file string) (line int) (code string)
 (rationale string) (editor-notify bool) (error-type int)
 (script-backtraces array))

(defgmethod
 (logger+-log-message :class 'logger :bind "_log_message" :hash 2678287736
  :virtual common-lisp:t)
 :void (message string) (error bool))