(common-lisp:in-package :%godot)


(defgmethod
 (animation-tree+set-tree-root :class 'animation-tree :bind "set_tree_root"
  :hash 2581683800)
 :void (animation-node animation-root-node))

(defgmethod
 (animation-tree+get-tree-root :class 'animation-tree :bind "get_tree_root"
  :hash 4110384712)
 animation-root-node)

(defgmethod
 (animation-tree+set-advance-expression-base-node :class 'animation-tree :bind
  "set_advance_expression_base_node" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (animation-tree+get-advance-expression-base-node :class 'animation-tree :bind
  "get_advance_expression_base_node" :hash 4075236667)
 node-path)

(defgmethod
 (animation-tree+set-animation-player :class 'animation-tree :bind
  "set_animation_player" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (animation-tree+get-animation-player :class 'animation-tree :bind
  "get_animation_player" :hash 4075236667)
 node-path)

(defgmethod
 (animation-tree+set-process-callback :class 'animation-tree :bind
  "set_process_callback" :hash 1723352826)
 :void (mode animation-tree+animation-process-callback))

(defgmethod
 (animation-tree+get-process-callback :class 'animation-tree :bind
  "get_process_callback" :hash 891317132)
 animation-tree+animation-process-callback)