(common-lisp:in-package :%godot)


(defgproperty visual-shader-node-compare+type 'visual-shader-node-compare :get
 'visual-shader-node-compare+get-comparison-type :set
 'visual-shader-node-compare+set-comparison-type)

(defgproperty visual-shader-node-compare+function 'visual-shader-node-compare
 :get 'visual-shader-node-compare+get-function :set
 'visual-shader-node-compare+set-function)

(defgproperty visual-shader-node-compare+condition 'visual-shader-node-compare
 :get 'visual-shader-node-compare+get-condition :set
 'visual-shader-node-compare+set-condition)