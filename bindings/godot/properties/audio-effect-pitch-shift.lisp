(common-lisp:in-package :%godot)


(defgproperty audio-effect-pitch-shift+pitch-scale 'audio-effect-pitch-shift
 :get 'audio-effect-pitch-shift+get-pitch-scale :set
 'audio-effect-pitch-shift+set-pitch-scale)

(defgproperty audio-effect-pitch-shift+oversampling 'audio-effect-pitch-shift
 :get 'audio-effect-pitch-shift+get-oversampling :set
 'audio-effect-pitch-shift+set-oversampling)

(defgproperty audio-effect-pitch-shift+fft-size 'audio-effect-pitch-shift :get
 'audio-effect-pitch-shift+get-fft-size :set
 'audio-effect-pitch-shift+set-fft-size)