(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-generator+set-mix-rate :class 'audio-stream-generator :bind
  "set_mix_rate" :hash 373806689)
 :void (hz float))

(defgmethod
 (audio-stream-generator+get-mix-rate :class 'audio-stream-generator :bind
  "get_mix_rate" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-generator+set-mix-rate-mode :class 'audio-stream-generator :bind
  "set_mix_rate_mode" :hash 3354885803)
 :void (mode audio-stream-generator+audio-stream-generator-mix-rate))

(defgmethod
 (audio-stream-generator+get-mix-rate-mode :class 'audio-stream-generator :bind
  "get_mix_rate_mode" :hash 3537132591)
 audio-stream-generator+audio-stream-generator-mix-rate)

(defgmethod
 (audio-stream-generator+set-buffer-length :class 'audio-stream-generator :bind
  "set_buffer_length" :hash 373806689)
 :void (seconds float))

(defgmethod
 (audio-stream-generator+get-buffer-length :class 'audio-stream-generator :bind
  "get_buffer_length" :hash 1740695150)
 float)