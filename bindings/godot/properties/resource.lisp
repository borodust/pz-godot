(common-lisp:in-package :%godot)


(defgproperty resource+resource-local-to-scene 'resource :get
 'resource+is-local-to-scene :set 'resource+set-local-to-scene)

(defgproperty resource+resource-path 'resource :get 'resource+get-path :set
 'resource+set-path)

(defgproperty resource+resource-name 'resource :get 'resource+get-name :set
 'resource+set-name)

(defgproperty resource+resource-scene-unique-id 'resource :get
 'resource+get-scene-unique-id :set 'resource+set-scene-unique-id)