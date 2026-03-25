(common-lisp:in-package :%godot)


(defgmethod
 (compressed-texture-3d+load :class 'compressed-texture-3d :bind "load" :hash
  166001499)
 error (path string))

(defgmethod
 (compressed-texture-3d+get-load-path :class 'compressed-texture-3d :bind
  "get_load_path" :hash 201670096)
 string)