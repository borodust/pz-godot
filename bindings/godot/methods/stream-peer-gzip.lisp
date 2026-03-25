(common-lisp:in-package :%godot)


(defgmethod
 (stream-peer-gzip+start-compression :class 'stream-peer-gzip :bind
  "start_compression" :hash 781582770)
 error (use-deflate bool) (buffer-size int))

(defgmethod
 (stream-peer-gzip+start-decompression :class 'stream-peer-gzip :bind
  "start_decompression" :hash 781582770)
 error (use-deflate bool) (buffer-size int))

(defgmethod
 (stream-peer-gzip+finish :class 'stream-peer-gzip :bind "finish" :hash
  166280745)
 error)

(defgmethod
 (stream-peer-gzip+clear :class 'stream-peer-gzip :bind "clear" :hash
  3218959716)
 :void)