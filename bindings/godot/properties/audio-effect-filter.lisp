(common-lisp:in-package :%godot)


(defgproperty audio-effect-filter+cutoff-hz 'audio-effect-filter :get
 'audio-effect-filter+get-cutoff :set 'audio-effect-filter+set-cutoff)

(defgproperty audio-effect-filter+resonance 'audio-effect-filter :get
 'audio-effect-filter+get-resonance :set 'audio-effect-filter+set-resonance)

(defgproperty audio-effect-filter+gain 'audio-effect-filter :get
 'audio-effect-filter+get-gain :set 'audio-effect-filter+set-gain)

(defgproperty audio-effect-filter+db 'audio-effect-filter :get
 'audio-effect-filter+get-db :set 'audio-effect-filter+set-db)