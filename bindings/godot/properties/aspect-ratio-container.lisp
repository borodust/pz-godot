(common-lisp:in-package :%godot)


(defgproperty aspect-ratio-container+ratio 'aspect-ratio-container :get
 'aspect-ratio-container+get-ratio :set 'aspect-ratio-container+set-ratio)

(defgproperty aspect-ratio-container+stretch-mode 'aspect-ratio-container :get
 'aspect-ratio-container+get-stretch-mode :set
 'aspect-ratio-container+set-stretch-mode)

(defgproperty aspect-ratio-container+alignment-horizontal
 'aspect-ratio-container :get 'aspect-ratio-container+get-alignment-horizontal
 :set 'aspect-ratio-container+set-alignment-horizontal)

(defgproperty aspect-ratio-container+alignment-vertical 'aspect-ratio-container
 :get 'aspect-ratio-container+get-alignment-vertical :set
 'aspect-ratio-container+set-alignment-vertical)