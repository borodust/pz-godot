(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-spectrum-analyzer-instance+get-magnitude-for-frequency-range
  :class 'audio-effect-spectrum-analyzer-instance :bind
  "get_magnitude_for_frequency_range" :hash 797993915)
 vector-2 (from-hz float) (to-hz float)
 (mode audio-effect-spectrum-analyzer-instance+magnitude-mode))