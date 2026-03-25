(common-lisp:in-package :%godot)


(defgproperty audio-effect-delay+dry 'audio-effect-delay :get
 'audio-effect-delay+get-dry :set 'audio-effect-delay+set-dry)

(defgproperty audio-effect-delay+tap1-active 'audio-effect-delay :get
 'audio-effect-delay+is-tap1-active :set 'audio-effect-delay+set-tap1-active)

(defgproperty audio-effect-delay+tap1-delay-ms 'audio-effect-delay :get
 'audio-effect-delay+get-tap1-delay-ms :set
 'audio-effect-delay+set-tap1-delay-ms)

(defgproperty audio-effect-delay+tap1-level-db 'audio-effect-delay :get
 'audio-effect-delay+get-tap1-level-db :set
 'audio-effect-delay+set-tap1-level-db)

(defgproperty audio-effect-delay+tap1-pan 'audio-effect-delay :get
 'audio-effect-delay+get-tap1-pan :set 'audio-effect-delay+set-tap1-pan)

(defgproperty audio-effect-delay+tap2-active 'audio-effect-delay :get
 'audio-effect-delay+is-tap2-active :set 'audio-effect-delay+set-tap2-active)

(defgproperty audio-effect-delay+tap2-delay-ms 'audio-effect-delay :get
 'audio-effect-delay+get-tap2-delay-ms :set
 'audio-effect-delay+set-tap2-delay-ms)

(defgproperty audio-effect-delay+tap2-level-db 'audio-effect-delay :get
 'audio-effect-delay+get-tap2-level-db :set
 'audio-effect-delay+set-tap2-level-db)

(defgproperty audio-effect-delay+tap2-pan 'audio-effect-delay :get
 'audio-effect-delay+get-tap2-pan :set 'audio-effect-delay+set-tap2-pan)

(defgproperty audio-effect-delay+feedback-active 'audio-effect-delay :get
 'audio-effect-delay+is-feedback-active :set
 'audio-effect-delay+set-feedback-active)

(defgproperty audio-effect-delay+feedback-delay-ms 'audio-effect-delay :get
 'audio-effect-delay+get-feedback-delay-ms :set
 'audio-effect-delay+set-feedback-delay-ms)

(defgproperty audio-effect-delay+feedback-level-db 'audio-effect-delay :get
 'audio-effect-delay+get-feedback-level-db :set
 'audio-effect-delay+set-feedback-level-db)

(defgproperty audio-effect-delay+feedback-lowpass 'audio-effect-delay :get
 'audio-effect-delay+get-feedback-lowpass :set
 'audio-effect-delay+set-feedback-lowpass)