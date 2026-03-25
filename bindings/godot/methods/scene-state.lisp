(common-lisp:in-package :%godot)


(defgmethod
 (scene-state+get-path :class 'scene-state :bind "get_path" :hash 201670096)
 string)

(defgmethod
 (scene-state+get-base-scene-state :class 'scene-state :bind
  "get_base_scene_state" :hash 3479783971)
 scene-state)

(defgmethod
 (scene-state+get-node-count :class 'scene-state :bind "get_node_count" :hash
  3905245786)
 int)

(defgmethod
 (scene-state+get-node-type :class 'scene-state :bind "get_node_type" :hash
  659327637)
 string-name (idx int))

(defgmethod
 (scene-state+get-node-name :class 'scene-state :bind "get_node_name" :hash
  659327637)
 string-name (idx int))

(defgmethod
 (scene-state+get-node-path :class 'scene-state :bind "get_node_path" :hash
  2272487792)
 node-path (idx int) (for-parent bool))

(defgmethod
 (scene-state+get-node-owner-path :class 'scene-state :bind
  "get_node_owner_path" :hash 408788394)
 node-path (idx int))

(defgmethod
 (scene-state+is-node-instance-placeholder :class 'scene-state :bind
  "is_node_instance_placeholder" :hash 1116898809)
 bool (idx int))

(defgmethod
 (scene-state+get-node-instance-placeholder :class 'scene-state :bind
  "get_node_instance_placeholder" :hash 844755477)
 string (idx int))

(defgmethod
 (scene-state+get-node-instance :class 'scene-state :bind "get_node_instance"
  :hash 511017218)
 packed-scene (idx int))

(defgmethod
 (scene-state+get-node-groups :class 'scene-state :bind "get_node_groups" :hash
  647634434)
 packed-string-array (idx int))

(defgmethod
 (scene-state+get-node-index :class 'scene-state :bind "get_node_index" :hash
  923996154)
 int (idx int))

(defgmethod
 (scene-state+get-node-property-count :class 'scene-state :bind
  "get_node_property_count" :hash 923996154)
 int (idx int))

(defgmethod
 (scene-state+get-node-property-name :class 'scene-state :bind
  "get_node_property_name" :hash 351665558)
 string-name (idx int) (prop-idx int))

(defgmethod
 (scene-state+get-node-property-value :class 'scene-state :bind
  "get_node_property_value" :hash 678354945)
 variant (idx int) (prop-idx int))

(defgmethod
 (scene-state+get-connection-count :class 'scene-state :bind
  "get_connection_count" :hash 3905245786)
 int)

(defgmethod
 (scene-state+get-connection-source :class 'scene-state :bind
  "get_connection_source" :hash 408788394)
 node-path (idx int))

(defgmethod
 (scene-state+get-connection-signal :class 'scene-state :bind
  "get_connection_signal" :hash 659327637)
 string-name (idx int))

(defgmethod
 (scene-state+get-connection-target :class 'scene-state :bind
  "get_connection_target" :hash 408788394)
 node-path (idx int))

(defgmethod
 (scene-state+get-connection-method :class 'scene-state :bind
  "get_connection_method" :hash 659327637)
 string-name (idx int))

(defgmethod
 (scene-state+get-connection-flags :class 'scene-state :bind
  "get_connection_flags" :hash 923996154)
 int (idx int))

(defgmethod
 (scene-state+get-connection-binds :class 'scene-state :bind
  "get_connection_binds" :hash 663333327)
 array (idx int))

(defgmethod
 (scene-state+get-connection-unbinds :class 'scene-state :bind
  "get_connection_unbinds" :hash 923996154)
 int (idx int))