(common-lisp:in-package :%godot)


(defgproperty visual-shader-node-parameter+parameter-name
 'visual-shader-node-parameter :get
 'visual-shader-node-parameter+get-parameter-name :set
 'visual-shader-node-parameter+set-parameter-name)

(defgproperty visual-shader-node-parameter+qualifier
 'visual-shader-node-parameter :get 'visual-shader-node-parameter+get-qualifier
 :set 'visual-shader-node-parameter+set-qualifier)

(defgproperty visual-shader-node-parameter+instance-index
 'visual-shader-node-parameter :get
 'visual-shader-node-parameter+get-instance-index :set
 'visual-shader-node-parameter+set-instance-index)