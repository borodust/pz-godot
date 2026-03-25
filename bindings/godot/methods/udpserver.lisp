(common-lisp:in-package :%godot)


(defgmethod
 (udpserver+listen :class 'udpserver :bind "listen" :hash 3167955072) error
 (port int) (bind-address string))

(defgmethod (udpserver+poll :class 'udpserver :bind "poll" :hash 166280745)
 error)

(defgmethod
 (udpserver+is-connection-available :class 'udpserver :bind
  "is_connection_available" :hash 36873697)
 bool)

(defgmethod
 (udpserver+get-local-port :class 'udpserver :bind "get_local_port" :hash
  3905245786)
 int)

(defgmethod
 (udpserver+is-listening :class 'udpserver :bind "is_listening" :hash 36873697)
 bool)

(defgmethod
 (udpserver+take-connection :class 'udpserver :bind "take_connection" :hash
  808734560)
 packet-peer-udp)

(defgmethod (udpserver+stop :class 'udpserver :bind "stop" :hash 3218959716)
 :void)

(defgmethod
 (udpserver+set-max-pending-connections :class 'udpserver :bind
  "set_max_pending_connections" :hash 1286410249)
 :void (max-pending-connections int))

(defgmethod
 (udpserver+get-max-pending-connections :class 'udpserver :bind
  "get_max_pending_connections" :hash 3905245786)
 int)