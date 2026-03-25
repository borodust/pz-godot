(common-lisp:in-package :%godot)


(defgmethod
 (rdpipeline-specialization-constant+set-value :class
  'rdpipeline-specialization-constant :bind "set_value" :hash 1114965689)
 :void (value variant))

(defgmethod
 (rdpipeline-specialization-constant+get-value :class
  'rdpipeline-specialization-constant :bind "get_value" :hash 1214101251)
 variant)

(defgmethod
 (rdpipeline-specialization-constant+set-constant-id :class
  'rdpipeline-specialization-constant :bind "set_constant_id" :hash 1286410249)
 :void (constant-id int))

(defgmethod
 (rdpipeline-specialization-constant+get-constant-id :class
  'rdpipeline-specialization-constant :bind "get_constant_id" :hash 3905245786)
 int)