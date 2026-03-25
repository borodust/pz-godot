(common-lisp:in-package :%godot)


(defgproperty audio-effect-amplify+volume-db 'audio-effect-amplify :get
 'audio-effect-amplify+get-volume-db :set 'audio-effect-amplify+set-volume-db)

(defgproperty audio-effect-amplify+volume-linear 'audio-effect-amplify :get
 'audio-effect-amplify+get-volume-linear :set
 'audio-effect-amplify+set-volume-linear)