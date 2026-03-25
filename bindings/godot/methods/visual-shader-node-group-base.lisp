(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-group-base+set-inputs :class
  'visual-shader-node-group-base :bind "set_inputs" :hash 83702148)
 :void (inputs string))

(defgmethod
 (visual-shader-node-group-base+get-inputs :class
  'visual-shader-node-group-base :bind "get_inputs" :hash 201670096)
 string)

(defgmethod
 (visual-shader-node-group-base+set-outputs :class
  'visual-shader-node-group-base :bind "set_outputs" :hash 83702148)
 :void (outputs string))

(defgmethod
 (visual-shader-node-group-base+get-outputs :class
  'visual-shader-node-group-base :bind "get_outputs" :hash 201670096)
 string)

(defgmethod
 (visual-shader-node-group-base+is-valid-port-name :class
  'visual-shader-node-group-base :bind "is_valid_port_name" :hash 3927539163)
 bool (name string))

(defgmethod
 (visual-shader-node-group-base+add-input-port :class
  'visual-shader-node-group-base :bind "add_input_port" :hash 2285447957)
 :void (id int) (type int) (name string))

(defgmethod
 (visual-shader-node-group-base+remove-input-port :class
  'visual-shader-node-group-base :bind "remove_input_port" :hash 1286410249)
 :void (id int))

(defgmethod
 (visual-shader-node-group-base+get-input-port-count :class
  'visual-shader-node-group-base :bind "get_input_port_count" :hash 3905245786)
 int)

(defgmethod
 (visual-shader-node-group-base+has-input-port :class
  'visual-shader-node-group-base :bind "has_input_port" :hash 1116898809)
 bool (id int))

(defgmethod
 (visual-shader-node-group-base+clear-input-ports :class
  'visual-shader-node-group-base :bind "clear_input_ports" :hash 3218959716)
 :void)

(defgmethod
 (visual-shader-node-group-base+add-output-port :class
  'visual-shader-node-group-base :bind "add_output_port" :hash 2285447957)
 :void (id int) (type int) (name string))

(defgmethod
 (visual-shader-node-group-base+remove-output-port :class
  'visual-shader-node-group-base :bind "remove_output_port" :hash 1286410249)
 :void (id int))

(defgmethod
 (visual-shader-node-group-base+get-output-port-count :class
  'visual-shader-node-group-base :bind "get_output_port_count" :hash
  3905245786)
 int)

(defgmethod
 (visual-shader-node-group-base+has-output-port :class
  'visual-shader-node-group-base :bind "has_output_port" :hash 1116898809)
 bool (id int))

(defgmethod
 (visual-shader-node-group-base+clear-output-ports :class
  'visual-shader-node-group-base :bind "clear_output_ports" :hash 3218959716)
 :void)

(defgmethod
 (visual-shader-node-group-base+set-input-port-name :class
  'visual-shader-node-group-base :bind "set_input_port_name" :hash 501894301)
 :void (id int) (name string))

(defgmethod
 (visual-shader-node-group-base+set-input-port-type :class
  'visual-shader-node-group-base :bind "set_input_port_type" :hash 3937882851)
 :void (id int) (type int))

(defgmethod
 (visual-shader-node-group-base+set-output-port-name :class
  'visual-shader-node-group-base :bind "set_output_port_name" :hash 501894301)
 :void (id int) (name string))

(defgmethod
 (visual-shader-node-group-base+set-output-port-type :class
  'visual-shader-node-group-base :bind "set_output_port_type" :hash 3937882851)
 :void (id int) (type int))

(defgmethod
 (visual-shader-node-group-base+get-free-input-port-id :class
  'visual-shader-node-group-base :bind "get_free_input_port_id" :hash
  3905245786)
 int)

(defgmethod
 (visual-shader-node-group-base+get-free-output-port-id :class
  'visual-shader-node-group-base :bind "get_free_output_port_id" :hash
  3905245786)
 int)