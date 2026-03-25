(common-lisp:in-package :%godot)


(defgmethod
 (stream-peer-tls+poll :class 'stream-peer-tls :bind "poll" :hash 3218959716)
 :void)

(defgmethod
 (stream-peer-tls+accept-stream :class 'stream-peer-tls :bind "accept_stream"
  :hash 4292689651)
 error (stream stream-peer) (server-options tlsoptions))

(defgmethod
 (stream-peer-tls+connect-to-stream :class 'stream-peer-tls :bind
  "connect_to_stream" :hash 57169517)
 error (stream stream-peer) (common-name string) (client-options tlsoptions))

(defgmethod
 (stream-peer-tls+get-status :class 'stream-peer-tls :bind "get_status" :hash
  1128380576)
 stream-peer-tls+status)

(defgmethod
 (stream-peer-tls+get-stream :class 'stream-peer-tls :bind "get_stream" :hash
  2741655269)
 stream-peer)

(defgmethod
 (stream-peer-tls+disconnect-from-stream :class 'stream-peer-tls :bind
  "disconnect_from_stream" :hash 3218959716)
 :void)