(common-lisp:in-package :%godot)


(defgmethod
 (packet-peer-udp+bind :class 'packet-peer-udp :bind "bind" :hash 4051239242)
 error (port int) (bind-address string) (recv-buf-size int))

(defgmethod
 (packet-peer-udp+close :class 'packet-peer-udp :bind "close" :hash 3218959716)
 :void)

(defgmethod
 (packet-peer-udp+wait :class 'packet-peer-udp :bind "wait" :hash 166280745)
 error)

(defgmethod
 (packet-peer-udp+is-bound :class 'packet-peer-udp :bind "is_bound" :hash
  36873697)
 bool)

(defgmethod
 (packet-peer-udp+connect-to-host :class 'packet-peer-udp :bind
  "connect_to_host" :hash 993915709)
 error (host string) (port int))

(defgmethod
 (packet-peer-udp+is-socket-connected :class 'packet-peer-udp :bind
  "is_socket_connected" :hash 36873697)
 bool)

(defgmethod
 (packet-peer-udp+get-packet-ip :class 'packet-peer-udp :bind "get_packet_ip"
  :hash 201670096)
 string)

(defgmethod
 (packet-peer-udp+get-packet-port :class 'packet-peer-udp :bind
  "get_packet_port" :hash 3905245786)
 int)

(defgmethod
 (packet-peer-udp+get-local-port :class 'packet-peer-udp :bind "get_local_port"
  :hash 3905245786)
 int)

(defgmethod
 (packet-peer-udp+set-dest-address :class 'packet-peer-udp :bind
  "set_dest_address" :hash 993915709)
 error (host string) (port int))

(defgmethod
 (packet-peer-udp+set-broadcast-enabled :class 'packet-peer-udp :bind
  "set_broadcast_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (packet-peer-udp+join-multicast-group :class 'packet-peer-udp :bind
  "join_multicast_group" :hash 852856452)
 error (multicast-address string) (interface-name string))

(defgmethod
 (packet-peer-udp+leave-multicast-group :class 'packet-peer-udp :bind
  "leave_multicast_group" :hash 852856452)
 error (multicast-address string) (interface-name string))