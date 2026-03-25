(common-lisp:in-package :%godot)


(defgproperty physics-material+friction 'physics-material :get
 'physics-material+get-friction :set 'physics-material+set-friction)

(defgproperty physics-material+rough 'physics-material :get
 'physics-material+is-rough :set 'physics-material+set-rough)

(defgproperty physics-material+bounce 'physics-material :get
 'physics-material+get-bounce :set 'physics-material+set-bounce)

(defgproperty physics-material+absorbent 'physics-material :get
 'physics-material+is-absorbent :set 'physics-material+set-absorbent)