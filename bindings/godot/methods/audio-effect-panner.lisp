(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-panner+set-pan :class 'audio-effect-panner :bind "set_pan" :hash
  373806689)
 :void (cpanume float))

(defgmethod
 (audio-effect-panner+get-pan :class 'audio-effect-panner :bind "get_pan" :hash
  1740695150)
 float)