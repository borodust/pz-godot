(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node+get-default-input-port :class 'visual-shader-node :bind
  "get_default_input_port" :hash 1894493699)
 int (type visual-shader-node+port-type))

(defgmethod
 (visual-shader-node+set-output-port-for-preview :class 'visual-shader-node
  :bind "set_output_port_for_preview" :hash 1286410249)
 :void (port int))

(defgmethod
 (visual-shader-node+get-output-port-for-preview :class 'visual-shader-node
  :bind "get_output_port_for_preview" :hash 3905245786)
 int)

(defgmethod
 (visual-shader-node+set-input-port-default-value :class 'visual-shader-node
  :bind "set_input_port_default_value" :hash 150923387)
 :void (port int) (value variant) (prev-value variant))

(defgmethod
 (visual-shader-node+get-input-port-default-value :class 'visual-shader-node
  :bind "get_input_port_default_value" :hash 4227898402)
 variant (port int))

(defgmethod
 (visual-shader-node+remove-input-port-default-value :class 'visual-shader-node
  :bind "remove_input_port_default_value" :hash 1286410249)
 :void (port int))

(defgmethod
 (visual-shader-node+clear-default-input-values :class 'visual-shader-node
  :bind "clear_default_input_values" :hash 3218959716)
 :void)

(defgmethod
 (visual-shader-node+set-default-input-values :class 'visual-shader-node :bind
  "set_default_input_values" :hash 381264803)
 :void (values array))

(defgmethod
 (visual-shader-node+get-default-input-values :class 'visual-shader-node :bind
  "get_default_input_values" :hash 3995934104)
 array)

(defgmethod
 (visual-shader-node+set-frame :class 'visual-shader-node :bind "set_frame"
  :hash 1286410249)
 :void (frame int))

(defgmethod
 (visual-shader-node+get-frame :class 'visual-shader-node :bind "get_frame"
  :hash 3905245786)
 int)