(common-lisp:in-package :%godot)


(defgmethod
 (instance-placeholder+get-stored-values :class 'instance-placeholder :bind
  "get_stored_values" :hash 2230153369)
 dictionary (with-order bool))

(defgmethod
 (instance-placeholder+create-instance :class 'instance-placeholder :bind
  "create_instance" :hash 3794612210)
 node (replace bool) (custom-scene packed-scene))

(defgmethod
 (instance-placeholder+get-instance-path :class 'instance-placeholder :bind
  "get_instance_path" :hash 201670096)
 string)