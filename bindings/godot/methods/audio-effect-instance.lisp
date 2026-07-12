(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-instance+%process :class 'audio-effect-instance :bind "_process"
  :hash 1649997291 :virtual common-lisp:t)
 :void (src-buffer (:pointer :void)) (r-dst-buffer (:pointer audio-frame))
 (frame-count int))

(defgmethod
 (audio-effect-instance+%process-silence :class 'audio-effect-instance :bind
  "_process_silence" :hash 36873697 :virtual common-lisp:t)
 bool)