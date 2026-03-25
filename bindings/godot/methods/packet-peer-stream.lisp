(common-lisp:in-package :%godot)


(defgmethod
 (packet-peer-stream+set-stream-peer :class 'packet-peer-stream :bind
  "set_stream_peer" :hash 3281897016)
 :void (peer stream-peer))

(defgmethod
 (packet-peer-stream+get-stream-peer :class 'packet-peer-stream :bind
  "get_stream_peer" :hash 2741655269)
 stream-peer)

(defgmethod
 (packet-peer-stream+set-input-buffer-max-size :class 'packet-peer-stream :bind
  "set_input_buffer_max_size" :hash 1286410249)
 :void (max-size-bytes int))

(defgmethod
 (packet-peer-stream+set-output-buffer-max-size :class 'packet-peer-stream
  :bind "set_output_buffer_max_size" :hash 1286410249)
 :void (max-size-bytes int))

(defgmethod
 (packet-peer-stream+get-input-buffer-max-size :class 'packet-peer-stream :bind
  "get_input_buffer_max_size" :hash 3905245786)
 int)

(defgmethod
 (packet-peer-stream+get-output-buffer-max-size :class 'packet-peer-stream
  :bind "get_output_buffer_max_size" :hash 3905245786)
 int)