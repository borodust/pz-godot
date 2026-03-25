(common-lisp:in-package :%godot)


(defgproperty web-socket-multiplayer-peer+supported-protocols
 'web-socket-multiplayer-peer :get
 'web-socket-multiplayer-peer+get-supported-protocols :set
 'web-socket-multiplayer-peer+set-supported-protocols)

(defgproperty web-socket-multiplayer-peer+handshake-headers
 'web-socket-multiplayer-peer :get
 'web-socket-multiplayer-peer+get-handshake-headers :set
 'web-socket-multiplayer-peer+set-handshake-headers)

(defgproperty web-socket-multiplayer-peer+inbound-buffer-size
 'web-socket-multiplayer-peer :get
 'web-socket-multiplayer-peer+get-inbound-buffer-size :set
 'web-socket-multiplayer-peer+set-inbound-buffer-size)

(defgproperty web-socket-multiplayer-peer+outbound-buffer-size
 'web-socket-multiplayer-peer :get
 'web-socket-multiplayer-peer+get-outbound-buffer-size :set
 'web-socket-multiplayer-peer+set-outbound-buffer-size)

(defgproperty web-socket-multiplayer-peer+handshake-timeout
 'web-socket-multiplayer-peer :get
 'web-socket-multiplayer-peer+get-handshake-timeout :set
 'web-socket-multiplayer-peer+set-handshake-timeout)

(defgproperty web-socket-multiplayer-peer+max-queued-packets
 'web-socket-multiplayer-peer :get
 'web-socket-multiplayer-peer+get-max-queued-packets :set
 'web-socket-multiplayer-peer+set-max-queued-packets)