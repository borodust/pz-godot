(common-lisp:in-package :%godot)


(defgproperty graph-node+title 'graph-node :get 'graph-node+get-title :set
 'graph-node+set-title)

(defgproperty graph-node+ignore-invalid-connection-type 'graph-node :get
 'graph-node+is-ignoring-valid-connection-type :set
 'graph-node+set-ignore-invalid-connection-type)

(defgproperty graph-node+slots-focus-mode 'graph-node :get
 'graph-node+get-slots-focus-mode :set 'graph-node+set-slots-focus-mode)