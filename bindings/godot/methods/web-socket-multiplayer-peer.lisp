(common-lisp:in-package :%godot)


(defgmethod
 (web-socket-multiplayer-peer+create-client :class 'web-socket-multiplayer-peer
  :bind "create_client" :hash 1966198364)
 error (url string) (tls-client-options tlsoptions))

(defgmethod
 (web-socket-multiplayer-peer+create-server :class 'web-socket-multiplayer-peer
  :bind "create_server" :hash 2400822951)
 error (port int) (bind-address string) (tls-server-options tlsoptions))

(defgmethod
 (web-socket-multiplayer-peer+get-peer :class 'web-socket-multiplayer-peer
  :bind "get_peer" :hash 1381378851)
 web-socket-peer (peer-id int))

(defgmethod
 (web-socket-multiplayer-peer+get-peer-address :class
  'web-socket-multiplayer-peer :bind "get_peer_address" :hash 844755477)
 string (id int))

(defgmethod
 (web-socket-multiplayer-peer+get-peer-port :class 'web-socket-multiplayer-peer
  :bind "get_peer_port" :hash 923996154)
 int (id int))

(defgmethod
 (web-socket-multiplayer-peer+get-supported-protocols :class
  'web-socket-multiplayer-peer :bind "get_supported_protocols" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (web-socket-multiplayer-peer+set-supported-protocols :class
  'web-socket-multiplayer-peer :bind "set_supported_protocols" :hash
  4015028928)
 :void (protocols packed-string-array))

(defgmethod
 (web-socket-multiplayer-peer+get-handshake-headers :class
  'web-socket-multiplayer-peer :bind "get_handshake_headers" :hash 1139954409)
 packed-string-array)

(defgmethod
 (web-socket-multiplayer-peer+set-handshake-headers :class
  'web-socket-multiplayer-peer :bind "set_handshake_headers" :hash 4015028928)
 :void (protocols packed-string-array))

(defgmethod
 (web-socket-multiplayer-peer+get-inbound-buffer-size :class
  'web-socket-multiplayer-peer :bind "get_inbound_buffer_size" :hash
  3905245786)
 int)

(defgmethod
 (web-socket-multiplayer-peer+set-inbound-buffer-size :class
  'web-socket-multiplayer-peer :bind "set_inbound_buffer_size" :hash
  1286410249)
 :void (buffer-size int))

(defgmethod
 (web-socket-multiplayer-peer+get-outbound-buffer-size :class
  'web-socket-multiplayer-peer :bind "get_outbound_buffer_size" :hash
  3905245786)
 int)

(defgmethod
 (web-socket-multiplayer-peer+set-outbound-buffer-size :class
  'web-socket-multiplayer-peer :bind "set_outbound_buffer_size" :hash
  1286410249)
 :void (buffer-size int))

(defgmethod
 (web-socket-multiplayer-peer+get-handshake-timeout :class
  'web-socket-multiplayer-peer :bind "get_handshake_timeout" :hash 1740695150)
 float)

(defgmethod
 (web-socket-multiplayer-peer+set-handshake-timeout :class
  'web-socket-multiplayer-peer :bind "set_handshake_timeout" :hash 373806689)
 :void (timeout float))

(defgmethod
 (web-socket-multiplayer-peer+set-max-queued-packets :class
  'web-socket-multiplayer-peer :bind "set_max_queued_packets" :hash 1286410249)
 :void (max-queued-packets int))

(defgmethod
 (web-socket-multiplayer-peer+get-max-queued-packets :class
  'web-socket-multiplayer-peer :bind "get_max_queued_packets" :hash 3905245786)
 int)