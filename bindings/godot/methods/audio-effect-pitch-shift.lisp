(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-pitch-shift+set-pitch-scale :class 'audio-effect-pitch-shift
  :bind "set_pitch_scale" :hash 373806689)
 :void (rate float))

(defgmethod
 (audio-effect-pitch-shift+get-pitch-scale :class 'audio-effect-pitch-shift
  :bind "get_pitch_scale" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-pitch-shift+set-oversampling :class 'audio-effect-pitch-shift
  :bind "set_oversampling" :hash 1286410249)
 :void (amount int))

(defgmethod
 (audio-effect-pitch-shift+get-oversampling :class 'audio-effect-pitch-shift
  :bind "get_oversampling" :hash 3905245786)
 int)

(defgmethod
 (audio-effect-pitch-shift+set-fft-size :class 'audio-effect-pitch-shift :bind
  "set_fft_size" :hash 2323518741)
 :void (size audio-effect-pitch-shift+fftsize))

(defgmethod
 (audio-effect-pitch-shift+get-fft-size :class 'audio-effect-pitch-shift :bind
  "get_fft_size" :hash 2361246789)
 audio-effect-pitch-shift+fftsize)