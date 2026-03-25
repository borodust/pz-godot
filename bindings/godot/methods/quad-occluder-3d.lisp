(common-lisp:in-package :%godot)


(defgmethod
 (quad-occluder-3d+set-size :class 'quad-occluder-3d :bind "set_size" :hash
  743155724)
 :void (size vector-2))

(defgmethod
 (quad-occluder-3d+get-size :class 'quad-occluder-3d :bind "get_size" :hash
  3341600327)
 vector-2)