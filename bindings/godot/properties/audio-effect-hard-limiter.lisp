(common-lisp:in-package :%godot)


(defgproperty audio-effect-hard-limiter+pre-gain-db 'audio-effect-hard-limiter
 :get 'audio-effect-hard-limiter+get-pre-gain-db :set
 'audio-effect-hard-limiter+set-pre-gain-db)

(defgproperty audio-effect-hard-limiter+ceiling-db 'audio-effect-hard-limiter
 :get 'audio-effect-hard-limiter+get-ceiling-db :set
 'audio-effect-hard-limiter+set-ceiling-db)

(defgproperty audio-effect-hard-limiter+release 'audio-effect-hard-limiter :get
 'audio-effect-hard-limiter+get-release :set
 'audio-effect-hard-limiter+set-release)