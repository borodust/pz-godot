(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-derivative-func+set-op-type :class
  'visual-shader-node-derivative-func :bind "set_op_type" :hash 377800221)
 :void (type visual-shader-node-derivative-func+op-type))

(defgmethod
 (visual-shader-node-derivative-func+get-op-type :class
  'visual-shader-node-derivative-func :bind "get_op_type" :hash 3997800514)
 visual-shader-node-derivative-func+op-type)

(defgmethod
 (visual-shader-node-derivative-func+set-function :class
  'visual-shader-node-derivative-func :bind "set_function" :hash 1944704156)
 :void (func visual-shader-node-derivative-func+function))

(defgmethod
 (visual-shader-node-derivative-func+get-function :class
  'visual-shader-node-derivative-func :bind "get_function" :hash 2389093396)
 visual-shader-node-derivative-func+function)

(defgmethod
 (visual-shader-node-derivative-func+set-precision :class
  'visual-shader-node-derivative-func :bind "set_precision" :hash 797270566)
 :void (precision visual-shader-node-derivative-func+precision))

(defgmethod
 (visual-shader-node-derivative-func+get-precision :class
  'visual-shader-node-derivative-func :bind "get_precision" :hash 3822547323)
 visual-shader-node-derivative-func+precision)