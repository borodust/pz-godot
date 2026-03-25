(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-spectrum-analyzer+set-buffer-length :class
  'audio-effect-spectrum-analyzer :bind "set_buffer_length" :hash 373806689)
 :void (seconds float))

(defgmethod
 (audio-effect-spectrum-analyzer+get-buffer-length :class
  'audio-effect-spectrum-analyzer :bind "get_buffer_length" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-spectrum-analyzer+set-tap-back-pos :class
  'audio-effect-spectrum-analyzer :bind "set_tap_back_pos" :hash 373806689)
 :void (seconds float))

(defgmethod
 (audio-effect-spectrum-analyzer+get-tap-back-pos :class
  'audio-effect-spectrum-analyzer :bind "get_tap_back_pos" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-spectrum-analyzer+set-fft-size :class
  'audio-effect-spectrum-analyzer :bind "set_fft_size" :hash 1202879215)
 :void (size audio-effect-spectrum-analyzer+fftsize))

(defgmethod
 (audio-effect-spectrum-analyzer+get-fft-size :class
  'audio-effect-spectrum-analyzer :bind "get_fft_size" :hash 3925405343)
 audio-effect-spectrum-analyzer+fftsize)