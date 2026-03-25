(common-lisp:in-package :%godot)


(defgproperty animation-tree+tree-root 'animation-tree :get
 'animation-tree+get-tree-root :set 'animation-tree+set-tree-root)

(defgproperty animation-tree+advance-expression-base-node 'animation-tree :get
 'animation-tree+get-advance-expression-base-node :set
 'animation-tree+set-advance-expression-base-node)

(defgproperty animation-tree+anim-player 'animation-tree :get
 'animation-tree+get-animation-player :set 'animation-tree+set-animation-player)