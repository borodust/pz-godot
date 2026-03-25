(common-lisp:in-package :%godot)


(defgmethod
 (encoded-object-as-id+set-object-id :class 'encoded-object-as-id :bind
  "set_object_id" :hash 1286410249)
 :void (id int))

(defgmethod
 (encoded-object-as-id+get-object-id :class 'encoded-object-as-id :bind
  "get_object_id" :hash 3905245786)
 int)