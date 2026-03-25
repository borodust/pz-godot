(common-lisp:in-package :%godot)


(defgmethod
 (stream-peer-socket+poll :class 'stream-peer-socket :bind "poll" :hash
  166280745)
 error)

(defgmethod
 (stream-peer-socket+get-status :class 'stream-peer-socket :bind "get_status"
  :hash 1156122502)
 stream-peer-socket+status)

(defgmethod
 (stream-peer-socket+disconnect-from-host :class 'stream-peer-socket :bind
  "disconnect_from_host" :hash 3218959716)
 :void)