(common-lisp:in-package :%godot)


(defgmethod
 (packet-peer+get-var :class 'packet-peer :bind "get_var" :hash 3442865206)
 variant (allow-objects bool))

(defgmethod
 (packet-peer+put-var :class 'packet-peer :bind "put_var" :hash 2436251611)
 error (var variant) (full-objects bool))

(defgmethod
 (packet-peer+get-packet :class 'packet-peer :bind "get_packet" :hash
  2115431945)
 packed-byte-array)

(defgmethod
 (packet-peer+put-packet :class 'packet-peer :bind "put_packet" :hash
  680677267)
 error (buffer packed-byte-array))

(defgmethod
 (packet-peer+get-packet-error :class 'packet-peer :bind "get_packet_error"
  :hash 3185525595)
 error)

(defgmethod
 (packet-peer+get-available-packet-count :class 'packet-peer :bind
  "get_available_packet_count" :hash 3905245786)
 int)

(defgmethod
 (packet-peer+get-encode-buffer-max-size :class 'packet-peer :bind
  "get_encode_buffer_max_size" :hash 3905245786)
 int)

(defgmethod
 (packet-peer+set-encode-buffer-max-size :class 'packet-peer :bind
  "set_encode_buffer_max_size" :hash 1286410249)
 :void (max-size int))