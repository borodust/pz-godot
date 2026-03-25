(common-lisp:in-package :%godot)


(defgmethod
 (compressed-texture-layered+load :class 'compressed-texture-layered :bind
  "load" :hash 166001499)
 error (path string))

(defgmethod
 (compressed-texture-layered+get-load-path :class 'compressed-texture-layered
  :bind "get_load_path" :hash 201670096)
 string)