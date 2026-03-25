(common-lisp:in-package :%godot)


(defgproperty audio-effect-stereo-enhance+pan-pullout
 'audio-effect-stereo-enhance :get 'audio-effect-stereo-enhance+get-pan-pullout
 :set 'audio-effect-stereo-enhance+set-pan-pullout)

(defgproperty audio-effect-stereo-enhance+time-pullout-ms
 'audio-effect-stereo-enhance :get
 'audio-effect-stereo-enhance+get-time-pullout :set
 'audio-effect-stereo-enhance+set-time-pullout)

(defgproperty audio-effect-stereo-enhance+surround 'audio-effect-stereo-enhance
 :get 'audio-effect-stereo-enhance+get-surround :set
 'audio-effect-stereo-enhance+set-surround)