(common-lisp:in-package :%godot)


(defgproperty audio-effect-limiter+ceiling-db 'audio-effect-limiter :get
 'audio-effect-limiter+get-ceiling-db :set 'audio-effect-limiter+set-ceiling-db)

(defgproperty audio-effect-limiter+threshold-db 'audio-effect-limiter :get
 'audio-effect-limiter+get-threshold-db :set
 'audio-effect-limiter+set-threshold-db)

(defgproperty audio-effect-limiter+soft-clip-db 'audio-effect-limiter :get
 'audio-effect-limiter+get-soft-clip-db :set
 'audio-effect-limiter+set-soft-clip-db)

(defgproperty audio-effect-limiter+soft-clip-ratio 'audio-effect-limiter :get
 'audio-effect-limiter+get-soft-clip-ratio :set
 'audio-effect-limiter+set-soft-clip-ratio)