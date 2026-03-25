(common-lisp:in-package :%godot)


(defgmethod
 (joint-2d+set-node-a :class 'joint-2d :bind "set_node_a" :hash 1348162250)
 :void (node node-path))

(defgmethod
 (joint-2d+get-node-a :class 'joint-2d :bind "get_node_a" :hash 4075236667)
 node-path)

(defgmethod
 (joint-2d+set-node-b :class 'joint-2d :bind "set_node_b" :hash 1348162250)
 :void (node node-path))

(defgmethod
 (joint-2d+get-node-b :class 'joint-2d :bind "get_node_b" :hash 4075236667)
 node-path)

(defgmethod
 (joint-2d+set-bias :class 'joint-2d :bind "set_bias" :hash 373806689) :void
 (bias float))

(defgmethod
 (joint-2d+get-bias :class 'joint-2d :bind "get_bias" :hash 1740695150) float)

(defgmethod
 (joint-2d+set-exclude-nodes-from-collision :class 'joint-2d :bind
  "set_exclude_nodes_from_collision" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (joint-2d+get-exclude-nodes-from-collision :class 'joint-2d :bind
  "get_exclude_nodes_from_collision" :hash 36873697)
 bool)

(defgmethod
 (joint-2d+get-rid :class 'joint-2d :bind "get_rid" :hash 2944877500) rid)