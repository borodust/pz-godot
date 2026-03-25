(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-sample-3d+set-source :class 'visual-shader-node-sample-3d
  :bind "set_source" :hash 3315130991)
 :void (value visual-shader-node-sample-3d+source))

(defgmethod
 (visual-shader-node-sample-3d+get-source :class 'visual-shader-node-sample-3d
  :bind "get_source" :hash 1079494121)
 visual-shader-node-sample-3d+source)