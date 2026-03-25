(common-lisp:in-package :%godot)


(defgmethod
 (editor-selection+clear :class 'editor-selection :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (editor-selection+add-node :class 'editor-selection :bind "add_node" :hash
  1078189570)
 :void (node node))

(defgmethod
 (editor-selection+remove-node :class 'editor-selection :bind "remove_node"
  :hash 1078189570)
 :void (node node))

(defgmethod
 (editor-selection+get-selected-nodes :class 'editor-selection :bind
  "get_selected_nodes" :hash 2915620761)
 array)

(defgmethod
 (editor-selection+get-top-selected-nodes :class 'editor-selection :bind
  "get_top_selected_nodes" :hash 2915620761)
 array)

(defgmethod
 (editor-selection+get-transformable-selected-nodes :class 'editor-selection
  :bind "get_transformable_selected_nodes" :hash 2915620761)
 array)