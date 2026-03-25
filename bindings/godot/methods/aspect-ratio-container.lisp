(common-lisp:in-package :%godot)


(defgmethod
 (aspect-ratio-container+set-ratio :class 'aspect-ratio-container :bind
  "set_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (aspect-ratio-container+get-ratio :class 'aspect-ratio-container :bind
  "get_ratio" :hash 1740695150)
 float)

(defgmethod
 (aspect-ratio-container+set-stretch-mode :class 'aspect-ratio-container :bind
  "set_stretch_mode" :hash 1876743467)
 :void (stretch-mode aspect-ratio-container+stretch-mode))

(defgmethod
 (aspect-ratio-container+get-stretch-mode :class 'aspect-ratio-container :bind
  "get_stretch_mode" :hash 3416449033)
 aspect-ratio-container+stretch-mode)

(defgmethod
 (aspect-ratio-container+set-alignment-horizontal :class
  'aspect-ratio-container :bind "set_alignment_horizontal" :hash 2147829016)
 :void (alignment-horizontal aspect-ratio-container+alignment-mode))

(defgmethod
 (aspect-ratio-container+get-alignment-horizontal :class
  'aspect-ratio-container :bind "get_alignment_horizontal" :hash 3838875429)
 aspect-ratio-container+alignment-mode)

(defgmethod
 (aspect-ratio-container+set-alignment-vertical :class 'aspect-ratio-container
  :bind "set_alignment_vertical" :hash 2147829016)
 :void (alignment-vertical aspect-ratio-container+alignment-mode))

(defgmethod
 (aspect-ratio-container+get-alignment-vertical :class 'aspect-ratio-container
  :bind "get_alignment_vertical" :hash 3838875429)
 aspect-ratio-container+alignment-mode)