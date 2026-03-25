(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-eq+set-band-gain-db :class 'audio-effect-eq :bind
  "set_band_gain_db" :hash 1602489585)
 :void (band-idx int) (volume-db float))

(defgmethod
 (audio-effect-eq+get-band-gain-db :class 'audio-effect-eq :bind
  "get_band_gain_db" :hash 2339986948)
 float (band-idx int))

(defgmethod
 (audio-effect-eq+get-band-count :class 'audio-effect-eq :bind "get_band_count"
  :hash 3905245786)
 int)