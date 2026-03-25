(common-lisp:in-package :%godot)


(defgmethod
 (scene-multiplayer+set-root-path :class 'scene-multiplayer :bind
  "set_root_path" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (scene-multiplayer+get-root-path :class 'scene-multiplayer :bind
  "get_root_path" :hash 4075236667)
 node-path)

(defgmethod
 (scene-multiplayer+clear :class 'scene-multiplayer :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (scene-multiplayer+disconnect-peer :class 'scene-multiplayer :bind
  "disconnect_peer" :hash 1286410249)
 :void (id int))

(defgmethod
 (scene-multiplayer+get-authenticating-peers :class 'scene-multiplayer :bind
  "get_authenticating_peers" :hash 969006518)
 packed-int-32array)

(defgmethod
 (scene-multiplayer+send-auth :class 'scene-multiplayer :bind "send_auth" :hash
  506032537)
 error (id int) (data packed-byte-array))

(defgmethod
 (scene-multiplayer+complete-auth :class 'scene-multiplayer :bind
  "complete_auth" :hash 844576869)
 error (id int))

(defgmethod
 (scene-multiplayer+set-auth-callback :class 'scene-multiplayer :bind
  "set_auth_callback" :hash 1611583062)
 :void (callback callable))

(defgmethod
 (scene-multiplayer+get-auth-callback :class 'scene-multiplayer :bind
  "get_auth_callback" :hash 1307783378)
 callable)

(defgmethod
 (scene-multiplayer+set-auth-timeout :class 'scene-multiplayer :bind
  "set_auth_timeout" :hash 373806689)
 :void (timeout float))

(defgmethod
 (scene-multiplayer+get-auth-timeout :class 'scene-multiplayer :bind
  "get_auth_timeout" :hash 1740695150)
 float)

(defgmethod
 (scene-multiplayer+set-refuse-new-connections :class 'scene-multiplayer :bind
  "set_refuse_new_connections" :hash 2586408642)
 :void (refuse bool))

(defgmethod
 (scene-multiplayer+is-refusing-new-connections :class 'scene-multiplayer :bind
  "is_refusing_new_connections" :hash 36873697)
 bool)

(defgmethod
 (scene-multiplayer+set-allow-object-decoding :class 'scene-multiplayer :bind
  "set_allow_object_decoding" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (scene-multiplayer+is-object-decoding-allowed :class 'scene-multiplayer :bind
  "is_object_decoding_allowed" :hash 36873697)
 bool)

(defgmethod
 (scene-multiplayer+set-server-relay-enabled :class 'scene-multiplayer :bind
  "set_server_relay_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (scene-multiplayer+is-server-relay-enabled :class 'scene-multiplayer :bind
  "is_server_relay_enabled" :hash 36873697)
 bool)

(defgmethod
 (scene-multiplayer+send-bytes :class 'scene-multiplayer :bind "send_bytes"
  :hash 1307428718)
 error (bytes packed-byte-array) (id int) (mode multiplayer-peer+transfer-mode)
 (channel int))

(defgmethod
 (scene-multiplayer+get-max-sync-packet-size :class 'scene-multiplayer :bind
  "get_max_sync_packet_size" :hash 3905245786)
 int)

(defgmethod
 (scene-multiplayer+set-max-sync-packet-size :class 'scene-multiplayer :bind
  "set_max_sync_packet_size" :hash 1286410249)
 :void (size int))

(defgmethod
 (scene-multiplayer+get-max-delta-packet-size :class 'scene-multiplayer :bind
  "get_max_delta_packet_size" :hash 3905245786)
 int)

(defgmethod
 (scene-multiplayer+set-max-delta-packet-size :class 'scene-multiplayer :bind
  "set_max_delta_packet_size" :hash 1286410249)
 :void (size int))