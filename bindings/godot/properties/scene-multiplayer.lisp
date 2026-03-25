(common-lisp:in-package :%godot)


(defgproperty scene-multiplayer+root-path 'scene-multiplayer :get
 'scene-multiplayer+get-root-path :set 'scene-multiplayer+set-root-path)

(defgproperty scene-multiplayer+auth-callback 'scene-multiplayer :get
 'scene-multiplayer+get-auth-callback :set 'scene-multiplayer+set-auth-callback)

(defgproperty scene-multiplayer+auth-timeout 'scene-multiplayer :get
 'scene-multiplayer+get-auth-timeout :set 'scene-multiplayer+set-auth-timeout)

(defgproperty scene-multiplayer+allow-object-decoding 'scene-multiplayer :get
 'scene-multiplayer+is-object-decoding-allowed :set
 'scene-multiplayer+set-allow-object-decoding)

(defgproperty scene-multiplayer+refuse-new-connections 'scene-multiplayer :get
 'scene-multiplayer+is-refusing-new-connections :set
 'scene-multiplayer+set-refuse-new-connections)

(defgproperty scene-multiplayer+server-relay 'scene-multiplayer :get
 'scene-multiplayer+is-server-relay-enabled :set
 'scene-multiplayer+set-server-relay-enabled)

(defgproperty scene-multiplayer+max-sync-packet-size 'scene-multiplayer :get
 'scene-multiplayer+get-max-sync-packet-size :set
 'scene-multiplayer+set-max-sync-packet-size)

(defgproperty scene-multiplayer+max-delta-packet-size 'scene-multiplayer :get
 'scene-multiplayer+get-max-delta-packet-size :set
 'scene-multiplayer+set-max-delta-packet-size)