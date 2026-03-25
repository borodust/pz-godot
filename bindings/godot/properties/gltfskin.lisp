(common-lisp:in-package :%godot)


(defgproperty gltfskin+skin-root 'gltfskin :get 'gltfskin+get-skin-root :set
 'gltfskin+set-skin-root)

(defgproperty gltfskin+joints-original 'gltfskin :get
 'gltfskin+get-joints-original :set 'gltfskin+set-joints-original)

(defgproperty gltfskin+inverse-binds 'gltfskin :get 'gltfskin+get-inverse-binds
 :set 'gltfskin+set-inverse-binds)

(defgproperty gltfskin+joints 'gltfskin :get 'gltfskin+get-joints :set
 'gltfskin+set-joints)

(defgproperty gltfskin+non-joints 'gltfskin :get 'gltfskin+get-non-joints :set
 'gltfskin+set-non-joints)

(defgproperty gltfskin+roots 'gltfskin :get 'gltfskin+get-roots :set
 'gltfskin+set-roots)

(defgproperty gltfskin+skeleton 'gltfskin :get 'gltfskin+get-skeleton :set
 'gltfskin+set-skeleton)

(defgproperty gltfskin+joint-i-to-bone-i 'gltfskin :get
 'gltfskin+get-joint-i-to-bone-i :set 'gltfskin+set-joint-i-to-bone-i)

(defgproperty gltfskin+joint-i-to-name 'gltfskin :get
 'gltfskin+get-joint-i-to-name :set 'gltfskin+set-joint-i-to-name)

(defgproperty gltfskin+godot-skin 'gltfskin :get 'gltfskin+get-godot-skin :set
 'gltfskin+set-godot-skin)