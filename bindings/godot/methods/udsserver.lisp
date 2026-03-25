(common-lisp:in-package :%godot)


(defgmethod (udsserver+listen :class 'udsserver :bind "listen" :hash 166001499)
 error (path string))

(defgmethod
 (udsserver+take-connection :class 'udsserver :bind "take_connection" :hash
  1623851112)
 stream-peer-uds)