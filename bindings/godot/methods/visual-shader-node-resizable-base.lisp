(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-resizable-base+set-size :class
  'visual-shader-node-resizable-base :bind "set_size" :hash 743155724)
 :void (size vector-2))

(defgmethod
 (visual-shader-node-resizable-base+get-size :class
  'visual-shader-node-resizable-base :bind "get_size" :hash 3341600327)
 vector-2)