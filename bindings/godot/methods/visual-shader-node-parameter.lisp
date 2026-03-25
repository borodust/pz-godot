(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-parameter+set-parameter-name :class
  'visual-shader-node-parameter :bind "set_parameter_name" :hash 83702148)
 :void (name string))

(defgmethod
 (visual-shader-node-parameter+get-parameter-name :class
  'visual-shader-node-parameter :bind "get_parameter_name" :hash 201670096)
 string)

(defgmethod
 (visual-shader-node-parameter+set-qualifier :class
  'visual-shader-node-parameter :bind "set_qualifier" :hash 1276489447)
 :void (qualifier visual-shader-node-parameter+qualifier))

(defgmethod
 (visual-shader-node-parameter+get-qualifier :class
  'visual-shader-node-parameter :bind "get_qualifier" :hash 3558406205)
 visual-shader-node-parameter+qualifier)

(defgmethod
 (visual-shader-node-parameter+set-instance-index :class
  'visual-shader-node-parameter :bind "set_instance_index" :hash 1286410249)
 :void (instance-index int))

(defgmethod
 (visual-shader-node-parameter+get-instance-index :class
  'visual-shader-node-parameter :bind "get_instance_index" :hash 3905245786)
 int)