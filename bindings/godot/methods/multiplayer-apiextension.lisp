(common-lisp:in-package :%godot)


(defgmethod
 (multiplayer-apiextension+-poll :class 'multiplayer-apiextension :bind "_poll"
  :hash 166280745 :virtual common-lisp:t)
 error)

(defgmethod
 (multiplayer-apiextension+-set-multiplayer-peer :class
  'multiplayer-apiextension :bind "_set_multiplayer_peer" :hash 3694835298
  :virtual common-lisp:t)
 :void (multiplayer-peer multiplayer-peer))

(defgmethod
 (multiplayer-apiextension+-get-multiplayer-peer :class
  'multiplayer-apiextension :bind "_get_multiplayer_peer" :hash 3223692825
  :virtual common-lisp:t)
 multiplayer-peer)

(defgmethod
 (multiplayer-apiextension+-get-unique-id :class 'multiplayer-apiextension
  :bind "_get_unique_id" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (multiplayer-apiextension+-get-peer-ids :class 'multiplayer-apiextension :bind
  "_get_peer_ids" :hash 1930428628 :virtual common-lisp:t)
 packed-int-32array)

(defgmethod
 (multiplayer-apiextension+-rpc :class 'multiplayer-apiextension :bind "_rpc"
  :hash 3673574758 :virtual common-lisp:t)
 error (peer int) (object object) (method string-name) (args array))

(defgmethod
 (multiplayer-apiextension+-get-remote-sender-id :class
  'multiplayer-apiextension :bind "_get_remote_sender_id" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (multiplayer-apiextension+-object-configuration-add :class
  'multiplayer-apiextension :bind "_object_configuration_add" :hash 1171879464
  :virtual common-lisp:t)
 error (object object) (configuration variant))

(defgmethod
 (multiplayer-apiextension+-object-configuration-remove :class
  'multiplayer-apiextension :bind "_object_configuration_remove" :hash
  1171879464 :virtual common-lisp:t)
 error (object object) (configuration variant))