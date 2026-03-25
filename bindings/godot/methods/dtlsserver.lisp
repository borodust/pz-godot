(common-lisp:in-package :%godot)


(defgmethod
 (dtlsserver+setup :class 'dtlsserver :bind "setup" :hash 1262296096) error
 (server-options tlsoptions))

(defgmethod
 (dtlsserver+take-connection :class 'dtlsserver :bind "take_connection" :hash
  3946580474)
 packet-peer-dtls (udp-peer packet-peer-udp))