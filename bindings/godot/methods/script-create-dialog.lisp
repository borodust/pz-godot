(common-lisp:in-package :%godot)


(defgmethod
 (script-create-dialog+config :class 'script-create-dialog :bind "config" :hash
  869314288)
 :void (inherits string) (path string) (built-in-enabled bool)
 (load-enabled bool))