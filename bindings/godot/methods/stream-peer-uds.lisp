(common-lisp:in-package :%godot)


(defgmethod
 (stream-peer-uds+bind :class 'stream-peer-uds :bind "bind" :hash 166001499)
 error (path string))

(defgmethod
 (stream-peer-uds+connect-to-host :class 'stream-peer-uds :bind
  "connect_to_host" :hash 166001499)
 error (path string))

(defgmethod
 (stream-peer-uds+get-connected-path :class 'stream-peer-uds :bind
  "get_connected_path" :hash 201670096)
 string)