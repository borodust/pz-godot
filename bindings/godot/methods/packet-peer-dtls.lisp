(common-lisp:in-package :%godot)


(defgmethod
 (packet-peer-dtls+poll :class 'packet-peer-dtls :bind "poll" :hash 3218959716)
 :void)

(defgmethod
 (packet-peer-dtls+connect-to-peer :class 'packet-peer-dtls :bind
  "connect_to_peer" :hash 2880188099)
 error (packet-peer packet-peer-udp) (hostname string)
 (client-options tlsoptions))

(defgmethod
 (packet-peer-dtls+get-status :class 'packet-peer-dtls :bind "get_status" :hash
  3248654679)
 packet-peer-dtls+status)

(defgmethod
 (packet-peer-dtls+disconnect-from-peer :class 'packet-peer-dtls :bind
  "disconnect_from_peer" :hash 3218959716)
 :void)