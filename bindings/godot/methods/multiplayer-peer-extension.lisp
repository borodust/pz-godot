(common-lisp:in-package :%godot)


(defgmethod
 (multiplayer-peer-extension+%get-packet :class 'multiplayer-peer-extension
  :bind "_get_packet" :hash 3099858825 :virtual common-lisp:t)
 error (r-buffer (:pointer (:pointer :uint8)))
 (r-buffer-size (:pointer :int32)))

(defgmethod
 (multiplayer-peer-extension+%put-packet :class 'multiplayer-peer-extension
  :bind "_put_packet" :hash 3099858825 :virtual common-lisp:t)
 error (p-buffer (:pointer :uint8)) (p-buffer-size int))

(defgmethod
 (multiplayer-peer-extension+%get-available-packet-count :class
  'multiplayer-peer-extension :bind "_get_available_packet_count" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (multiplayer-peer-extension+%get-max-packet-size :class
  'multiplayer-peer-extension :bind "_get_max_packet_size" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (multiplayer-peer-extension+%get-packet-script :class
  'multiplayer-peer-extension :bind "_get_packet_script" :hash 2115431945
  :virtual common-lisp:t)
 packed-byte-array)

(defgmethod
 (multiplayer-peer-extension+%put-packet-script :class
  'multiplayer-peer-extension :bind "_put_packet_script" :hash 680677267
  :virtual common-lisp:t)
 error (p-buffer packed-byte-array))

(defgmethod
 (multiplayer-peer-extension+%get-packet-channel :class
  'multiplayer-peer-extension :bind "_get_packet_channel" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (multiplayer-peer-extension+%get-packet-mode :class
  'multiplayer-peer-extension :bind "_get_packet_mode" :hash 3369852622
  :virtual common-lisp:t)
 multiplayer-peer+transfer-mode)

(defgmethod
 (multiplayer-peer-extension+%set-transfer-channel :class
  'multiplayer-peer-extension :bind "_set_transfer_channel" :hash 1286410249
  :virtual common-lisp:t)
 :void (p-channel int))

(defgmethod
 (multiplayer-peer-extension+%get-transfer-channel :class
  'multiplayer-peer-extension :bind "_get_transfer_channel" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (multiplayer-peer-extension+%set-transfer-mode :class
  'multiplayer-peer-extension :bind "_set_transfer_mode" :hash 950411049
  :virtual common-lisp:t)
 :void (p-mode multiplayer-peer+transfer-mode))

(defgmethod
 (multiplayer-peer-extension+%get-transfer-mode :class
  'multiplayer-peer-extension :bind "_get_transfer_mode" :hash 3369852622
  :virtual common-lisp:t)
 multiplayer-peer+transfer-mode)

(defgmethod
 (multiplayer-peer-extension+%set-target-peer :class
  'multiplayer-peer-extension :bind "_set_target_peer" :hash 1286410249
  :virtual common-lisp:t)
 :void (p-peer int))

(defgmethod
 (multiplayer-peer-extension+%get-packet-peer :class
  'multiplayer-peer-extension :bind "_get_packet_peer" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (multiplayer-peer-extension+%is-server :class 'multiplayer-peer-extension
  :bind "_is_server" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (multiplayer-peer-extension+%poll :class 'multiplayer-peer-extension :bind
  "_poll" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (multiplayer-peer-extension+%close :class 'multiplayer-peer-extension :bind
  "_close" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (multiplayer-peer-extension+%disconnect-peer :class
  'multiplayer-peer-extension :bind "_disconnect_peer" :hash 300928843 :virtual
  common-lisp:t)
 :void (p-peer int) (p-force bool))

(defgmethod
 (multiplayer-peer-extension+%get-unique-id :class 'multiplayer-peer-extension
  :bind "_get_unique_id" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (multiplayer-peer-extension+%set-refuse-new-connections :class
  'multiplayer-peer-extension :bind "_set_refuse_new_connections" :hash
  2586408642 :virtual common-lisp:t)
 :void (p-enable bool))

(defgmethod
 (multiplayer-peer-extension+%is-refusing-new-connections :class
  'multiplayer-peer-extension :bind "_is_refusing_new_connections" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (multiplayer-peer-extension+%is-server-relay-supported :class
  'multiplayer-peer-extension :bind "_is_server_relay_supported" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (multiplayer-peer-extension+%get-connection-status :class
  'multiplayer-peer-extension :bind "_get_connection_status" :hash 2147374275
  :virtual common-lisp:t)
 multiplayer-peer+connection-status)