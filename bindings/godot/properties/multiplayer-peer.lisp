(common-lisp:in-package :%godot)


(defgproperty multiplayer-peer+refuse-new-connections 'multiplayer-peer :get
 'multiplayer-peer+is-refusing-new-connections :set
 'multiplayer-peer+set-refuse-new-connections)

(defgproperty multiplayer-peer+transfer-mode 'multiplayer-peer :get
 'multiplayer-peer+get-transfer-mode :set 'multiplayer-peer+set-transfer-mode)

(defgproperty multiplayer-peer+transfer-channel 'multiplayer-peer :get
 'multiplayer-peer+get-transfer-channel :set
 'multiplayer-peer+set-transfer-channel)