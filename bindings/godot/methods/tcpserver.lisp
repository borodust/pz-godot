(common-lisp:in-package :%godot)


(defgmethod
 (tcpserver+listen :class 'tcpserver :bind "listen" :hash 3167955072) error
 (port int) (bind-address string))

(defgmethod
 (tcpserver+get-local-port :class 'tcpserver :bind "get_local_port" :hash
  3905245786)
 int)

(defgmethod
 (tcpserver+take-connection :class 'tcpserver :bind "take_connection" :hash
  30545006)
 stream-peer-tcp)