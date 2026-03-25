(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-custom+-get-name :class 'visual-shader-node-custom :bind
  "_get_name" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (visual-shader-node-custom+-get-description :class 'visual-shader-node-custom
  :bind "_get_description" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (visual-shader-node-custom+-get-category :class 'visual-shader-node-custom
  :bind "_get_category" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (visual-shader-node-custom+-get-return-icon-type :class
  'visual-shader-node-custom :bind "_get_return_icon_type" :hash 1287173294
  :virtual common-lisp:t)
 visual-shader-node+port-type)

(defgmethod
 (visual-shader-node-custom+-get-input-port-count :class
  'visual-shader-node-custom :bind "_get_input_port_count" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (visual-shader-node-custom+-get-input-port-type :class
  'visual-shader-node-custom :bind "_get_input_port_type" :hash 4102573379
  :virtual common-lisp:t)
 visual-shader-node+port-type (port int))

(defgmethod
 (visual-shader-node-custom+-get-input-port-name :class
  'visual-shader-node-custom :bind "_get_input_port_name" :hash 844755477
  :virtual common-lisp:t)
 string (port int))

(defgmethod
 (visual-shader-node-custom+-get-input-port-default-value :class
  'visual-shader-node-custom :bind "_get_input_port_default_value" :hash
  4227898402 :virtual common-lisp:t)
 variant (port int))

(defgmethod
 (visual-shader-node-custom+-get-default-input-port :class
  'visual-shader-node-custom :bind "_get_default_input_port" :hash 1894493699
  :virtual common-lisp:t)
 int (type visual-shader-node+port-type))

(defgmethod
 (visual-shader-node-custom+-get-output-port-count :class
  'visual-shader-node-custom :bind "_get_output_port_count" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (visual-shader-node-custom+-get-output-port-type :class
  'visual-shader-node-custom :bind "_get_output_port_type" :hash 4102573379
  :virtual common-lisp:t)
 visual-shader-node+port-type (port int))

(defgmethod
 (visual-shader-node-custom+-get-output-port-name :class
  'visual-shader-node-custom :bind "_get_output_port_name" :hash 844755477
  :virtual common-lisp:t)
 string (port int))

(defgmethod
 (visual-shader-node-custom+-get-property-count :class
  'visual-shader-node-custom :bind "_get_property_count" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (visual-shader-node-custom+-get-property-name :class
  'visual-shader-node-custom :bind "_get_property_name" :hash 844755477
  :virtual common-lisp:t)
 string (index int))

(defgmethod
 (visual-shader-node-custom+-get-property-default-index :class
  'visual-shader-node-custom :bind "_get_property_default_index" :hash
  923996154 :virtual common-lisp:t)
 int (index int))

(defgmethod
 (visual-shader-node-custom+-get-property-options :class
  'visual-shader-node-custom :bind "_get_property_options" :hash 647634434
  :virtual common-lisp:t)
 packed-string-array (index int))

(defgmethod
 (visual-shader-node-custom+-get-code :class 'visual-shader-node-custom :bind
  "_get_code" :hash 4287175357 :virtual common-lisp:t)
 string (input-vars array) (output-vars array) (mode shader+mode)
 (type visual-shader+type))

(defgmethod
 (visual-shader-node-custom+-get-func-code :class 'visual-shader-node-custom
  :bind "_get_func_code" :hash 1924221678 :virtual common-lisp:t)
 string (mode shader+mode) (type visual-shader+type))

(defgmethod
 (visual-shader-node-custom+-get-global-code :class 'visual-shader-node-custom
  :bind "_get_global_code" :hash 3956542358 :virtual common-lisp:t)
 string (mode shader+mode))

(defgmethod
 (visual-shader-node-custom+-is-highend :class 'visual-shader-node-custom :bind
  "_is_highend" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (visual-shader-node-custom+-is-available :class 'visual-shader-node-custom
  :bind "_is_available" :hash 1932120545 :virtual common-lisp:t)
 bool (mode shader+mode) (type visual-shader+type))

(defgmethod
 (visual-shader-node-custom+get-option-index :class 'visual-shader-node-custom
  :bind "get_option_index" :hash 923996154)
 int (option int))