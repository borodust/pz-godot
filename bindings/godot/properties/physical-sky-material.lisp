(common-lisp:in-package :%godot)


(defgproperty physical-sky-material+rayleigh-coefficient 'physical-sky-material
 :get 'physical-sky-material+get-rayleigh-coefficient :set
 'physical-sky-material+set-rayleigh-coefficient)

(defgproperty physical-sky-material+rayleigh-color 'physical-sky-material :get
 'physical-sky-material+get-rayleigh-color :set
 'physical-sky-material+set-rayleigh-color)

(defgproperty physical-sky-material+mie-coefficient 'physical-sky-material :get
 'physical-sky-material+get-mie-coefficient :set
 'physical-sky-material+set-mie-coefficient)

(defgproperty physical-sky-material+mie-eccentricity 'physical-sky-material
 :get 'physical-sky-material+get-mie-eccentricity :set
 'physical-sky-material+set-mie-eccentricity)

(defgproperty physical-sky-material+mie-color 'physical-sky-material :get
 'physical-sky-material+get-mie-color :set 'physical-sky-material+set-mie-color)

(defgproperty physical-sky-material+turbidity 'physical-sky-material :get
 'physical-sky-material+get-turbidity :set 'physical-sky-material+set-turbidity)

(defgproperty physical-sky-material+sun-disk-scale 'physical-sky-material :get
 'physical-sky-material+get-sun-disk-scale :set
 'physical-sky-material+set-sun-disk-scale)

(defgproperty physical-sky-material+ground-color 'physical-sky-material :get
 'physical-sky-material+get-ground-color :set
 'physical-sky-material+set-ground-color)

(defgproperty physical-sky-material+energy-multiplier 'physical-sky-material
 :get 'physical-sky-material+get-energy-multiplier :set
 'physical-sky-material+set-energy-multiplier)

(defgproperty physical-sky-material+use-debanding 'physical-sky-material :get
 'physical-sky-material+get-use-debanding :set
 'physical-sky-material+set-use-debanding)

(defgproperty physical-sky-material+night-sky 'physical-sky-material :get
 'physical-sky-material+get-night-sky :set 'physical-sky-material+set-night-sky)