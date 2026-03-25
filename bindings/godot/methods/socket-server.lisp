(common-lisp:in-package :%godot)


(defgmethod
 (socket-server+is-connection-available :class 'socket-server :bind
  "is_connection_available" :hash 36873697)
 bool)

(defgmethod
 (socket-server+is-listening :class 'socket-server :bind "is_listening" :hash
  36873697)
 bool)

(defgmethod
 (socket-server+stop :class 'socket-server :bind "stop" :hash 3218959716) :void)

(defgmethod
 (socket-server+take-socket-connection :class 'socket-server :bind
  "take_socket_connection" :hash 1883962599)
 stream-peer-socket)