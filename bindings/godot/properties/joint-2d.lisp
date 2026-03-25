(common-lisp:in-package :%godot)


(defgproperty joint-2d+node-a 'joint-2d :get 'joint-2d+get-node-a :set
 'joint-2d+set-node-a)

(defgproperty joint-2d+node-b 'joint-2d :get 'joint-2d+get-node-b :set
 'joint-2d+set-node-b)

(defgproperty joint-2d+bias 'joint-2d :get 'joint-2d+get-bias :set
 'joint-2d+set-bias)

(defgproperty joint-2d+disable-collision 'joint-2d :get
 'joint-2d+get-exclude-nodes-from-collision :set
 'joint-2d+set-exclude-nodes-from-collision)