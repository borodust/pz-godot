(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-blend-tree+add-node :class 'animation-node-blend-tree :bind
  "add_node" :hash 1980270704)
 :void (name string-name) (node animation-node) (position vector-2))

(defgmethod
 (animation-node-blend-tree+get-node :class 'animation-node-blend-tree :bind
  "get_node" :hash 625644256)
 animation-node (name string-name))

(defgmethod
 (animation-node-blend-tree+remove-node :class 'animation-node-blend-tree :bind
  "remove_node" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (animation-node-blend-tree+rename-node :class 'animation-node-blend-tree :bind
  "rename_node" :hash 3740211285)
 :void (name string-name) (new-name string-name))

(defgmethod
 (animation-node-blend-tree+has-node :class 'animation-node-blend-tree :bind
  "has_node" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (animation-node-blend-tree+connect-node :class 'animation-node-blend-tree
  :bind "connect_node" :hash 2168001410)
 :void (input-node string-name) (input-index int) (output-node string-name))

(defgmethod
 (animation-node-blend-tree+disconnect-node :class 'animation-node-blend-tree
  :bind "disconnect_node" :hash 2415702435)
 :void (input-node string-name) (input-index int))

(defgmethod
 (animation-node-blend-tree+get-node-list :class 'animation-node-blend-tree
  :bind "get_node_list" :hash 3995934104)
 array)

(defgmethod
 (animation-node-blend-tree+set-node-position :class 'animation-node-blend-tree
  :bind "set_node_position" :hash 1999414630)
 :void (name string-name) (position vector-2))

(defgmethod
 (animation-node-blend-tree+get-node-position :class 'animation-node-blend-tree
  :bind "get_node_position" :hash 3100822709)
 vector-2 (name string-name))

(defgmethod
 (animation-node-blend-tree+set-graph-offset :class 'animation-node-blend-tree
  :bind "set_graph_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (animation-node-blend-tree+get-graph-offset :class 'animation-node-blend-tree
  :bind "get_graph_offset" :hash 3341600327)
 vector-2)