(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-stereo-enhance+set-pan-pullout :class
  'audio-effect-stereo-enhance :bind "set_pan_pullout" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-stereo-enhance+get-pan-pullout :class
  'audio-effect-stereo-enhance :bind "get_pan_pullout" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-stereo-enhance+set-time-pullout :class
  'audio-effect-stereo-enhance :bind "set_time_pullout" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-stereo-enhance+get-time-pullout :class
  'audio-effect-stereo-enhance :bind "get_time_pullout" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-stereo-enhance+set-surround :class 'audio-effect-stereo-enhance
  :bind "set_surround" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-stereo-enhance+get-surround :class 'audio-effect-stereo-enhance
  :bind "get_surround" :hash 1740695150)
 float)