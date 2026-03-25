(common-lisp:in-package :%godot)


(defgmethod
 (stream-peer-tcp+bind :class 'stream-peer-tcp :bind "bind" :hash 3167955072)
 error (port int) (host string))

(defgmethod
 (stream-peer-tcp+connect-to-host :class 'stream-peer-tcp :bind
  "connect_to_host" :hash 993915709)
 error (host string) (port int))

(defgmethod
 (stream-peer-tcp+get-connected-host :class 'stream-peer-tcp :bind
  "get_connected_host" :hash 201670096)
 string)

(defgmethod
 (stream-peer-tcp+get-connected-port :class 'stream-peer-tcp :bind
  "get_connected_port" :hash 3905245786)
 int)

(defgmethod
 (stream-peer-tcp+get-local-port :class 'stream-peer-tcp :bind "get_local_port"
  :hash 3905245786)
 int)

(defgmethod
 (stream-peer-tcp+set-no-delay :class 'stream-peer-tcp :bind "set_no_delay"
  :hash 2586408642)
 :void (enabled bool))