(common-lisp:in-package :%godot)


(defgmethod
 (multiplayer-api+has-multiplayer-peer :class 'multiplayer-api :bind
  "has_multiplayer_peer" :hash 2240911060)
 bool)

(defgmethod
 (multiplayer-api+get-multiplayer-peer :class 'multiplayer-api :bind
  "get_multiplayer_peer" :hash 3223692825)
 multiplayer-peer)

(defgmethod
 (multiplayer-api+set-multiplayer-peer :class 'multiplayer-api :bind
  "set_multiplayer_peer" :hash 3694835298)
 :void (peer multiplayer-peer))

(defgmethod
 (multiplayer-api+get-unique-id :class 'multiplayer-api :bind "get_unique_id"
  :hash 2455072627)
 int)

(defgmethod
 (multiplayer-api+is-server :class 'multiplayer-api :bind "is_server" :hash
  2240911060)
 bool)

(defgmethod
 (multiplayer-api+get-remote-sender-id :class 'multiplayer-api :bind
  "get_remote_sender_id" :hash 2455072627)
 int)

(defgmethod
 (multiplayer-api+poll :class 'multiplayer-api :bind "poll" :hash 166280745)
 error)

(defgmethod
 (multiplayer-api+rpc :class 'multiplayer-api :bind "rpc" :hash 2077486355)
 error (peer int) (object object) (method string-name) (arguments array))

(defgmethod
 (multiplayer-api+object-configuration-add :class 'multiplayer-api :bind
  "object_configuration_add" :hash 1171879464)
 error (object object) (configuration variant))

(defgmethod
 (multiplayer-api+object-configuration-remove :class 'multiplayer-api :bind
  "object_configuration_remove" :hash 1171879464)
 error (object object) (configuration variant))

(defgmethod
 (multiplayer-api+get-peers :class 'multiplayer-api :bind "get_peers" :hash
  969006518)
 packed-int-32array)

(defgmethod
 (multiplayer-api+set-default-interface :class 'multiplayer-api :bind
  "set_default_interface" :hash 3304788590 :static common-lisp:t)
 :void (interface-name string-name))

(defgmethod
 (multiplayer-api+get-default-interface :class 'multiplayer-api :bind
  "get_default_interface" :hash 2737447660 :static common-lisp:t)
 string-name)

(defgmethod
 (multiplayer-api+create-default-interface :class 'multiplayer-api :bind
  "create_default_interface" :hash 3294156723 :static common-lisp:t)
 multiplayer-api)