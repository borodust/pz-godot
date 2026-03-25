(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-amplify+set-volume-db :class 'audio-effect-amplify :bind
  "set_volume_db" :hash 373806689)
 :void (volume float))

(defgmethod
 (audio-effect-amplify+get-volume-db :class 'audio-effect-amplify :bind
  "get_volume_db" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-amplify+set-volume-linear :class 'audio-effect-amplify :bind
  "set_volume_linear" :hash 373806689)
 :void (volume float))

(defgmethod
 (audio-effect-amplify+get-volume-linear :class 'audio-effect-amplify :bind
  "get_volume_linear" :hash 1740695150)
 float)