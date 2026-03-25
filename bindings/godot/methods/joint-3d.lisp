(common-lisp:in-package :%godot)


(defgmethod
 (joint-3d+set-node-a :class 'joint-3d :bind "set_node_a" :hash 1348162250)
 :void (node node-path))

(defgmethod
 (joint-3d+get-node-a :class 'joint-3d :bind "get_node_a" :hash 4075236667)
 node-path)

(defgmethod
 (joint-3d+set-node-b :class 'joint-3d :bind "set_node_b" :hash 1348162250)
 :void (node node-path))

(defgmethod
 (joint-3d+get-node-b :class 'joint-3d :bind "get_node_b" :hash 4075236667)
 node-path)

(defgmethod
 (joint-3d+set-solver-priority :class 'joint-3d :bind "set_solver_priority"
  :hash 1286410249)
 :void (priority int))

(defgmethod
 (joint-3d+get-solver-priority :class 'joint-3d :bind "get_solver_priority"
  :hash 3905245786)
 int)

(defgmethod
 (joint-3d+set-exclude-nodes-from-collision :class 'joint-3d :bind
  "set_exclude_nodes_from_collision" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (joint-3d+get-exclude-nodes-from-collision :class 'joint-3d :bind
  "get_exclude_nodes_from_collision" :hash 36873697)
 bool)

(defgmethod
 (joint-3d+get-rid :class 'joint-3d :bind "get_rid" :hash 2944877500) rid)