(common-lisp:in-package :%godot)


(defgmethod
 (shader-include+set-code :class 'shader-include :bind "set_code" :hash
  83702148)
 :void (code string))

(defgmethod
 (shader-include+get-code :class 'shader-include :bind "get_code" :hash
  201670096)
 string)