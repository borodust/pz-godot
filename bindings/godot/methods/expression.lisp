(common-lisp:in-package :%godot)


(defgmethod
 (expression+parse :class 'expression :bind "parse" :hash 3069722906) error
 (expression string) (input-names packed-string-array))

(defgmethod
 (expression+execute :class 'expression :bind "execute" :hash 3712471238)
 variant (inputs array) (base-instance object) (show-error bool)
 (const-calls-only bool))

(defgmethod
 (expression+has-execute-failed :class 'expression :bind "has_execute_failed"
  :hash 36873697)
 bool)

(defgmethod
 (expression+get-error-text :class 'expression :bind "get_error_text" :hash
  201670096)
 string)