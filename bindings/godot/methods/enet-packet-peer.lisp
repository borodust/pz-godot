(common-lisp:in-package :%godot)


(defgmethod
 (enet-packet-peer+peer-disconnect :class 'enet-packet-peer :bind
  "peer_disconnect" :hash 1995695955)
 :void (data int))

(defgmethod
 (enet-packet-peer+peer-disconnect-later :class 'enet-packet-peer :bind
  "peer_disconnect_later" :hash 1995695955)
 :void (data int))

(defgmethod
 (enet-packet-peer+peer-disconnect-now :class 'enet-packet-peer :bind
  "peer_disconnect_now" :hash 1995695955)
 :void (data int))

(defgmethod
 (enet-packet-peer+ping :class 'enet-packet-peer :bind "ping" :hash 3218959716)
 :void)

(defgmethod
 (enet-packet-peer+ping-interval :class 'enet-packet-peer :bind "ping_interval"
  :hash 1286410249)
 :void (ping-interval int))

(defgmethod
 (enet-packet-peer+reset :class 'enet-packet-peer :bind "reset" :hash
  3218959716)
 :void)

(defgmethod
 (enet-packet-peer+send :class 'enet-packet-peer :bind "send" :hash 120522849)
 error (channel int) (packet packed-byte-array) (flags int))

(defgmethod
 (enet-packet-peer+throttle-configure :class 'enet-packet-peer :bind
  "throttle_configure" :hash 1649997291)
 :void (interval int) (acceleration int) (deceleration int))

(defgmethod
 (enet-packet-peer+set-timeout :class 'enet-packet-peer :bind "set_timeout"
  :hash 1649997291)
 :void (timeout int) (timeout-min int) (timeout-max int))

(defgmethod
 (enet-packet-peer+get-packet-flags :class 'enet-packet-peer :bind
  "get_packet_flags" :hash 3905245786)
 int)

(defgmethod
 (enet-packet-peer+get-remote-address :class 'enet-packet-peer :bind
  "get_remote_address" :hash 201670096)
 string)

(defgmethod
 (enet-packet-peer+get-remote-port :class 'enet-packet-peer :bind
  "get_remote_port" :hash 3905245786)
 int)

(defgmethod
 (enet-packet-peer+get-statistic :class 'enet-packet-peer :bind "get_statistic"
  :hash 1642578323)
 float (statistic enet-packet-peer+peer-statistic))

(defgmethod
 (enet-packet-peer+get-state :class 'enet-packet-peer :bind "get_state" :hash
  711068532)
 enet-packet-peer+peer-state)

(defgmethod
 (enet-packet-peer+get-channels :class 'enet-packet-peer :bind "get_channels"
  :hash 3905245786)
 int)

(defgmethod
 (enet-packet-peer+is-active :class 'enet-packet-peer :bind "is_active" :hash
  36873697)
 bool)