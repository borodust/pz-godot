(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-limiter+set-ceiling-db :class 'audio-effect-limiter :bind
  "set_ceiling_db" :hash 373806689)
 :void (ceiling float))

(defgmethod
 (audio-effect-limiter+get-ceiling-db :class 'audio-effect-limiter :bind
  "get_ceiling_db" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-limiter+set-threshold-db :class 'audio-effect-limiter :bind
  "set_threshold_db" :hash 373806689)
 :void (threshold float))

(defgmethod
 (audio-effect-limiter+get-threshold-db :class 'audio-effect-limiter :bind
  "get_threshold_db" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-limiter+set-soft-clip-db :class 'audio-effect-limiter :bind
  "set_soft_clip_db" :hash 373806689)
 :void (soft-clip float))

(defgmethod
 (audio-effect-limiter+get-soft-clip-db :class 'audio-effect-limiter :bind
  "get_soft_clip_db" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-limiter+set-soft-clip-ratio :class 'audio-effect-limiter :bind
  "set_soft_clip_ratio" :hash 373806689)
 :void (soft-clip float))

(defgmethod
 (audio-effect-limiter+get-soft-clip-ratio :class 'audio-effect-limiter :bind
  "get_soft_clip_ratio" :hash 1740695150)
 float)