(common-lisp:in-package :%godot)


(defgproperty joint-3d+node-a 'joint-3d :get 'joint-3d+get-node-a :set
 'joint-3d+set-node-a)

(defgproperty joint-3d+node-b 'joint-3d :get 'joint-3d+get-node-b :set
 'joint-3d+set-node-b)

(defgproperty joint-3d+solver-priority 'joint-3d :get
 'joint-3d+get-solver-priority :set 'joint-3d+set-solver-priority)

(defgproperty joint-3d+exclude-nodes-from-collision 'joint-3d :get
 'joint-3d+get-exclude-nodes-from-collision :set
 'joint-3d+set-exclude-nodes-from-collision)