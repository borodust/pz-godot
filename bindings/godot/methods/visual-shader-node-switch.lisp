(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-switch+set-op-type :class 'visual-shader-node-switch :bind
  "set_op_type" :hash 510471861)
 :void (type visual-shader-node-switch+op-type))

(defgmethod
 (visual-shader-node-switch+get-op-type :class 'visual-shader-node-switch :bind
  "get_op_type" :hash 2517845071)
 visual-shader-node-switch+op-type)