(common-lisp:in-package :%godot)


(defgproperty panorama-sky-material+panorama 'panorama-sky-material :get
 'panorama-sky-material+get-panorama :set 'panorama-sky-material+set-panorama)

(defgproperty panorama-sky-material+filter 'panorama-sky-material :get
 'panorama-sky-material+is-filtering-enabled :set
 'panorama-sky-material+set-filtering-enabled)

(defgproperty panorama-sky-material+energy-multiplier 'panorama-sky-material
 :get 'panorama-sky-material+get-energy-multiplier :set
 'panorama-sky-material+set-energy-multiplier)