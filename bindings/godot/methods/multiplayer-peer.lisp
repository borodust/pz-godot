(common-lisp:in-package :%godot)


(defgmethod
 (multiplayer-peer+set-transfer-channel :class 'multiplayer-peer :bind
  "set_transfer_channel" :hash 1286410249)
 :void (channel int))

(defgmethod
 (multiplayer-peer+get-transfer-channel :class 'multiplayer-peer :bind
  "get_transfer_channel" :hash 3905245786)
 int)

(defgmethod
 (multiplayer-peer+set-transfer-mode :class 'multiplayer-peer :bind
  "set_transfer_mode" :hash 950411049)
 :void (mode multiplayer-peer+transfer-mode))

(defgmethod
 (multiplayer-peer+get-transfer-mode :class 'multiplayer-peer :bind
  "get_transfer_mode" :hash 3369852622)
 multiplayer-peer+transfer-mode)

(defgmethod
 (multiplayer-peer+set-target-peer :class 'multiplayer-peer :bind
  "set_target_peer" :hash 1286410249)
 :void (id int))

(defgmethod
 (multiplayer-peer+get-packet-peer :class 'multiplayer-peer :bind
  "get_packet_peer" :hash 3905245786)
 int)

(defgmethod
 (multiplayer-peer+get-packet-channel :class 'multiplayer-peer :bind
  "get_packet_channel" :hash 3905245786)
 int)

(defgmethod
 (multiplayer-peer+get-packet-mode :class 'multiplayer-peer :bind
  "get_packet_mode" :hash 3369852622)
 multiplayer-peer+transfer-mode)

(defgmethod
 (multiplayer-peer+poll :class 'multiplayer-peer :bind "poll" :hash 3218959716)
 :void)

(defgmethod
 (multiplayer-peer+close :class 'multiplayer-peer :bind "close" :hash
  3218959716)
 :void)

(defgmethod
 (multiplayer-peer+disconnect-peer :class 'multiplayer-peer :bind
  "disconnect_peer" :hash 4023243586)
 :void (peer int) (force bool))

(defgmethod
 (multiplayer-peer+get-connection-status :class 'multiplayer-peer :bind
  "get_connection_status" :hash 2147374275)
 multiplayer-peer+connection-status)

(defgmethod
 (multiplayer-peer+get-unique-id :class 'multiplayer-peer :bind "get_unique_id"
  :hash 3905245786)
 int)

(defgmethod
 (multiplayer-peer+generate-unique-id :class 'multiplayer-peer :bind
  "generate_unique_id" :hash 3905245786)
 int)

(defgmethod
 (multiplayer-peer+set-refuse-new-connections :class 'multiplayer-peer :bind
  "set_refuse_new_connections" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (multiplayer-peer+is-refusing-new-connections :class 'multiplayer-peer :bind
  "is_refusing_new_connections" :hash 36873697)
 bool)

(defgmethod
 (multiplayer-peer+is-server-relay-supported :class 'multiplayer-peer :bind
  "is_server_relay_supported" :hash 36873697)
 bool)