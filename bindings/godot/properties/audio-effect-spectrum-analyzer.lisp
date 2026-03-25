(common-lisp:in-package :%godot)


(defgproperty audio-effect-spectrum-analyzer+buffer-length
 'audio-effect-spectrum-analyzer :get
 'audio-effect-spectrum-analyzer+get-buffer-length :set
 'audio-effect-spectrum-analyzer+set-buffer-length)

(defgproperty audio-effect-spectrum-analyzer+tap-back-pos
 'audio-effect-spectrum-analyzer :get
 'audio-effect-spectrum-analyzer+get-tap-back-pos :set
 'audio-effect-spectrum-analyzer+set-tap-back-pos)

(defgproperty audio-effect-spectrum-analyzer+fft-size
 'audio-effect-spectrum-analyzer :get
 'audio-effect-spectrum-analyzer+get-fft-size :set
 'audio-effect-spectrum-analyzer+set-fft-size)