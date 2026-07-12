(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-hard-limiter+set-ceiling-db :class 'audio-effect-hard-limiter
  :bind "set_ceiling_db" :hash 373806689)
 :void (ceiling float))

(defgmethod
 (audio-effect-hard-limiter+get-ceiling-db :class 'audio-effect-hard-limiter
  :bind "get_ceiling_db" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-hard-limiter+set-pre-gain-db :class 'audio-effect-hard-limiter
  :bind "set_pre_gain_db" :hash 373806689)
 :void (pre-gain float))

(defgmethod
 (audio-effect-hard-limiter+get-pre-gain-db :class 'audio-effect-hard-limiter
  :bind "get_pre_gain_db" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-hard-limiter+set-release :class 'audio-effect-hard-limiter :bind
  "set_release" :hash 373806689)
 :void (release float))

(defgmethod
 (audio-effect-hard-limiter+get-release :class 'audio-effect-hard-limiter :bind
  "get_release" :hash 1740695150)
 float)