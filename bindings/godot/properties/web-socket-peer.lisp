(common-lisp:in-package :%godot)


(defgproperty web-socket-peer+supported-protocols 'web-socket-peer :get
 'web-socket-peer+get-supported-protocols :set
 'web-socket-peer+set-supported-protocols)

(defgproperty web-socket-peer+handshake-headers 'web-socket-peer :get
 'web-socket-peer+get-handshake-headers :set
 'web-socket-peer+set-handshake-headers)

(defgproperty web-socket-peer+inbound-buffer-size 'web-socket-peer :get
 'web-socket-peer+get-inbound-buffer-size :set
 'web-socket-peer+set-inbound-buffer-size)

(defgproperty web-socket-peer+outbound-buffer-size 'web-socket-peer :get
 'web-socket-peer+get-outbound-buffer-size :set
 'web-socket-peer+set-outbound-buffer-size)

(defgproperty web-socket-peer+max-queued-packets 'web-socket-peer :get
 'web-socket-peer+get-max-queued-packets :set
 'web-socket-peer+set-max-queued-packets)

(defgproperty web-socket-peer+heartbeat-interval 'web-socket-peer :get
 'web-socket-peer+get-heartbeat-interval :set
 'web-socket-peer+set-heartbeat-interval)