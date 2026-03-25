(common-lisp:in-package :%godot)


(defgproperty node+name 'node :get 'node+get-name :set 'node+set-name)

(defgproperty node+unique-name-in-owner 'node :get
 'node+is-unique-name-in-owner :set 'node+set-unique-name-in-owner)

(defgproperty node+scene-file-path 'node :get 'node+get-scene-file-path :set
 'node+set-scene-file-path)

(defgproperty node+owner 'node :get 'node+get-owner :set 'node+set-owner)

(defgproperty node+multiplayer 'node :get 'node+get-multiplayer)

(defgproperty node+process-mode 'node :get 'node+get-process-mode :set
 'node+set-process-mode)

(defgproperty node+process-priority 'node :get 'node+get-process-priority :set
 'node+set-process-priority)

(defgproperty node+process-physics-priority 'node :get
 'node+get-physics-process-priority :set 'node+set-physics-process-priority)

(defgproperty node+process-thread-group 'node :get
 'node+get-process-thread-group :set 'node+set-process-thread-group)

(defgproperty node+process-thread-group-order 'node :get
 'node+get-process-thread-group-order :set 'node+set-process-thread-group-order)

(defgproperty node+process-thread-messages 'node :get
 'node+get-process-thread-messages :set 'node+set-process-thread-messages)

(defgproperty node+physics-interpolation-mode 'node :get
 'node+get-physics-interpolation-mode :set 'node+set-physics-interpolation-mode)

(defgproperty node+auto-translate-mode 'node :get 'node+get-auto-translate-mode
 :set 'node+set-auto-translate-mode)

(defgproperty node+editor-description 'node :get 'node+get-editor-description
 :set 'node+set-editor-description)