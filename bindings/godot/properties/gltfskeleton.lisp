(common-lisp:in-package :%godot)


(defgproperty gltfskeleton+joints 'gltfskeleton :get 'gltfskeleton+get-joints
 :set 'gltfskeleton+set-joints)

(defgproperty gltfskeleton+roots 'gltfskeleton :get 'gltfskeleton+get-roots
 :set 'gltfskeleton+set-roots)

(defgproperty gltfskeleton+unique-names 'gltfskeleton :get
 'gltfskeleton+get-unique-names :set 'gltfskeleton+set-unique-names)

(defgproperty gltfskeleton+godot-bone-node 'gltfskeleton :get
 'gltfskeleton+get-godot-bone-node :set 'gltfskeleton+set-godot-bone-node)