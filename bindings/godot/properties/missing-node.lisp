(common-lisp:in-package :%godot)


(defgproperty missing-node+original-class 'missing-node :get
 'missing-node+get-original-class :set 'missing-node+set-original-class)

(defgproperty missing-node+original-scene 'missing-node :get
 'missing-node+get-original-scene :set 'missing-node+set-original-scene)

(defgproperty missing-node+recording-properties 'missing-node :get
 'missing-node+is-recording-properties :set
 'missing-node+set-recording-properties)

(defgproperty missing-node+recording-signals 'missing-node :get
 'missing-node+is-recording-signals :set 'missing-node+set-recording-signals)