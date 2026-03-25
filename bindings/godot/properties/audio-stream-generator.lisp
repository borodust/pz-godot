(common-lisp:in-package :%godot)


(defgproperty audio-stream-generator+mix-rate-mode 'audio-stream-generator :get
 'audio-stream-generator+get-mix-rate-mode :set
 'audio-stream-generator+set-mix-rate-mode)

(defgproperty audio-stream-generator+mix-rate 'audio-stream-generator :get
 'audio-stream-generator+get-mix-rate :set 'audio-stream-generator+set-mix-rate)

(defgproperty audio-stream-generator+buffer-length 'audio-stream-generator :get
 'audio-stream-generator+get-buffer-length :set
 'audio-stream-generator+set-buffer-length)