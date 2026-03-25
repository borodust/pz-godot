(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-state-machine+add-node :class 'animation-node-state-machine
  :bind "add_node" :hash 1980270704)
 :void (name string-name) (node animation-node) (position vector-2))

(defgmethod
 (animation-node-state-machine+replace-node :class
  'animation-node-state-machine :bind "replace_node" :hash 2559412862)
 :void (name string-name) (node animation-node))

(defgmethod
 (animation-node-state-machine+get-node :class 'animation-node-state-machine
  :bind "get_node" :hash 625644256)
 animation-node (name string-name))

(defgmethod
 (animation-node-state-machine+remove-node :class 'animation-node-state-machine
  :bind "remove_node" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (animation-node-state-machine+rename-node :class 'animation-node-state-machine
  :bind "rename_node" :hash 3740211285)
 :void (name string-name) (new-name string-name))

(defgmethod
 (animation-node-state-machine+has-node :class 'animation-node-state-machine
  :bind "has_node" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (animation-node-state-machine+get-node-name :class
  'animation-node-state-machine :bind "get_node_name" :hash 739213945)
 string-name (node animation-node))

(defgmethod
 (animation-node-state-machine+get-node-list :class
  'animation-node-state-machine :bind "get_node_list" :hash 3995934104)
 array)

(defgmethod
 (animation-node-state-machine+set-node-position :class
  'animation-node-state-machine :bind "set_node_position" :hash 1999414630)
 :void (name string-name) (position vector-2))

(defgmethod
 (animation-node-state-machine+get-node-position :class
  'animation-node-state-machine :bind "get_node_position" :hash 3100822709)
 vector-2 (name string-name))

(defgmethod
 (animation-node-state-machine+has-transition :class
  'animation-node-state-machine :bind "has_transition" :hash 471820014)
 bool (from string-name) (to string-name))

(defgmethod
 (animation-node-state-machine+add-transition :class
  'animation-node-state-machine :bind "add_transition" :hash 795486887)
 :void (from string-name) (to string-name)
 (transition animation-node-state-machine-transition))

(defgmethod
 (animation-node-state-machine+get-transition :class
  'animation-node-state-machine :bind "get_transition" :hash 4192381260)
 animation-node-state-machine-transition (idx int))

(defgmethod
 (animation-node-state-machine+get-transition-from :class
  'animation-node-state-machine :bind "get_transition_from" :hash 659327637)
 string-name (idx int))

(defgmethod
 (animation-node-state-machine+get-transition-to :class
  'animation-node-state-machine :bind "get_transition_to" :hash 659327637)
 string-name (idx int))

(defgmethod
 (animation-node-state-machine+get-transition-count :class
  'animation-node-state-machine :bind "get_transition_count" :hash 3905245786)
 int)

(defgmethod
 (animation-node-state-machine+remove-transition-by-index :class
  'animation-node-state-machine :bind "remove_transition_by_index" :hash
  1286410249)
 :void (idx int))

(defgmethod
 (animation-node-state-machine+remove-transition :class
  'animation-node-state-machine :bind "remove_transition" :hash 3740211285)
 :void (from string-name) (to string-name))

(defgmethod
 (animation-node-state-machine+set-graph-offset :class
  'animation-node-state-machine :bind "set_graph_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (animation-node-state-machine+get-graph-offset :class
  'animation-node-state-machine :bind "get_graph_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (animation-node-state-machine+set-state-machine-type :class
  'animation-node-state-machine :bind "set_state_machine_type" :hash
  2584759088)
 :void (state-machine-type animation-node-state-machine+state-machine-type))

(defgmethod
 (animation-node-state-machine+get-state-machine-type :class
  'animation-node-state-machine :bind "get_state_machine_type" :hash
  1140726469)
 animation-node-state-machine+state-machine-type)

(defgmethod
 (animation-node-state-machine+set-allow-transition-to-self :class
  'animation-node-state-machine :bind "set_allow_transition_to_self" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-state-machine+is-allow-transition-to-self :class
  'animation-node-state-machine :bind "is_allow_transition_to_self" :hash
  36873697)
 bool)

(defgmethod
 (animation-node-state-machine+set-reset-ends :class
  'animation-node-state-machine :bind "set_reset_ends" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-state-machine+are-ends-reset :class
  'animation-node-state-machine :bind "are_ends_reset" :hash 36873697)
 bool)