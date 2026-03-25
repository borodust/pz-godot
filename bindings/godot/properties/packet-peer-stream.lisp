(common-lisp:in-package :%godot)


(defgproperty packet-peer-stream+input-buffer-max-size 'packet-peer-stream :get
 'packet-peer-stream+get-input-buffer-max-size :set
 'packet-peer-stream+set-input-buffer-max-size)

(defgproperty packet-peer-stream+output-buffer-max-size 'packet-peer-stream
 :get 'packet-peer-stream+get-output-buffer-max-size :set
 'packet-peer-stream+set-output-buffer-max-size)

(defgproperty packet-peer-stream+stream-peer 'packet-peer-stream :get
 'packet-peer-stream+get-stream-peer :set 'packet-peer-stream+set-stream-peer)