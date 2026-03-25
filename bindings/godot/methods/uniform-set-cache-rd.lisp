(common-lisp:in-package :%godot)


(defgmethod
 (uniform-set-cache-rd+get-cache :class 'uniform-set-cache-rd :bind "get_cache"
  :hash 658571723 :static common-lisp:t)
 rid (shader rid) (set int) (uniforms array))