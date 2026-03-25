(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader+set-mode :class 'visual-shader :bind "set_mode" :hash
  3978014962)
 :void (mode shader+mode))

(defgmethod
 (visual-shader+add-node :class 'visual-shader :bind "add_node" :hash
  1560769431)
 :void (type visual-shader+type) (node visual-shader-node) (position vector-2)
 (id int))

(defgmethod
 (visual-shader+get-node :class 'visual-shader :bind "get_node" :hash
  3784670312)
 visual-shader-node (type visual-shader+type) (id int))

(defgmethod
 (visual-shader+set-node-position :class 'visual-shader :bind
  "set_node_position" :hash 2726660721)
 :void (type visual-shader+type) (id int) (position vector-2))

(defgmethod
 (visual-shader+get-node-position :class 'visual-shader :bind
  "get_node_position" :hash 2175036082)
 vector-2 (type visual-shader+type) (id int))

(defgmethod
 (visual-shader+get-node-list :class 'visual-shader :bind "get_node_list" :hash
  2370592410)
 packed-int-32array (type visual-shader+type))

(defgmethod
 (visual-shader+get-valid-node-id :class 'visual-shader :bind
  "get_valid_node_id" :hash 629467342)
 int (type visual-shader+type))

(defgmethod
 (visual-shader+remove-node :class 'visual-shader :bind "remove_node" :hash
  844050912)
 :void (type visual-shader+type) (id int))

(defgmethod
 (visual-shader+replace-node :class 'visual-shader :bind "replace_node" :hash
  3144735253)
 :void (type visual-shader+type) (id int) (new-class string-name))

(defgmethod
 (visual-shader+is-node-connection :class 'visual-shader :bind
  "is_node_connection" :hash 3922381898)
 bool (type visual-shader+type) (from-node int) (from-port int) (to-node int)
 (to-port int))

(defgmethod
 (visual-shader+can-connect-nodes :class 'visual-shader :bind
  "can_connect_nodes" :hash 3922381898)
 bool (type visual-shader+type) (from-node int) (from-port int) (to-node int)
 (to-port int))

(defgmethod
 (visual-shader+connect-nodes :class 'visual-shader :bind "connect_nodes" :hash
  3081049573)
 error (type visual-shader+type) (from-node int) (from-port int) (to-node int)
 (to-port int))

(defgmethod
 (visual-shader+disconnect-nodes :class 'visual-shader :bind "disconnect_nodes"
  :hash 2268060358)
 :void (type visual-shader+type) (from-node int) (from-port int) (to-node int)
 (to-port int))

(defgmethod
 (visual-shader+connect-nodes-forced :class 'visual-shader :bind
  "connect_nodes_forced" :hash 2268060358)
 :void (type visual-shader+type) (from-node int) (from-port int) (to-node int)
 (to-port int))

(defgmethod
 (visual-shader+get-node-connections :class 'visual-shader :bind
  "get_node_connections" :hash 1441964831)
 array (type visual-shader+type))

(defgmethod
 (visual-shader+attach-node-to-frame :class 'visual-shader :bind
  "attach_node_to_frame" :hash 2479945279)
 :void (type visual-shader+type) (id int) (frame int))

(defgmethod
 (visual-shader+detach-node-from-frame :class 'visual-shader :bind
  "detach_node_from_frame" :hash 844050912)
 :void (type visual-shader+type) (id int))

(defgmethod
 (visual-shader+add-varying :class 'visual-shader :bind "add_varying" :hash
  2084110726)
 :void (name string) (mode visual-shader+varying-mode)
 (type visual-shader+varying-type))

(defgmethod
 (visual-shader+remove-varying :class 'visual-shader :bind "remove_varying"
  :hash 83702148)
 :void (name string))

(defgmethod
 (visual-shader+has-varying :class 'visual-shader :bind "has_varying" :hash
  3927539163)
 bool (name string))

(defgmethod
 (visual-shader+set-graph-offset :class 'visual-shader :bind "set_graph_offset"
  :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (visual-shader+get-graph-offset :class 'visual-shader :bind "get_graph_offset"
  :hash 3341600327)
 vector-2)