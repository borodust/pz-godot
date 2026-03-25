(common-lisp:in-package :%godot)


(defgmethod
 (placeholder-texture-3d+set-size :class 'placeholder-texture-3d :bind
  "set_size" :hash 560364750)
 :void (size vector-3i))

(defgmethod
 (placeholder-texture-3d+get-size :class 'placeholder-texture-3d :bind
  "get_size" :hash 2785653706)
 vector-3i)