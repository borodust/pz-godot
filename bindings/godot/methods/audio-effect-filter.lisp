(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-filter+set-cutoff :class 'audio-effect-filter :bind "set_cutoff"
  :hash 373806689)
 :void (freq float))

(defgmethod
 (audio-effect-filter+get-cutoff :class 'audio-effect-filter :bind "get_cutoff"
  :hash 1740695150)
 float)

(defgmethod
 (audio-effect-filter+set-resonance :class 'audio-effect-filter :bind
  "set_resonance" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-filter+get-resonance :class 'audio-effect-filter :bind
  "get_resonance" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-filter+set-gain :class 'audio-effect-filter :bind "set_gain"
  :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-filter+get-gain :class 'audio-effect-filter :bind "get_gain"
  :hash 1740695150)
 float)

(defgmethod
 (audio-effect-filter+set-db :class 'audio-effect-filter :bind "set_db" :hash
  771740901)
 :void (amount audio-effect-filter+filter-db))

(defgmethod
 (audio-effect-filter+get-db :class 'audio-effect-filter :bind "get_db" :hash
  3981721890)
 audio-effect-filter+filter-db)