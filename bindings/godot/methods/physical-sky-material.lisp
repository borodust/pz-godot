(common-lisp:in-package :%godot)


(defgmethod
 (physical-sky-material+set-rayleigh-coefficient :class 'physical-sky-material
  :bind "set_rayleigh_coefficient" :hash 373806689)
 :void (rayleigh float))

(defgmethod
 (physical-sky-material+get-rayleigh-coefficient :class 'physical-sky-material
  :bind "get_rayleigh_coefficient" :hash 1740695150)
 float)

(defgmethod
 (physical-sky-material+set-rayleigh-color :class 'physical-sky-material :bind
  "set_rayleigh_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (physical-sky-material+get-rayleigh-color :class 'physical-sky-material :bind
  "get_rayleigh_color" :hash 3444240500)
 color)

(defgmethod
 (physical-sky-material+set-mie-coefficient :class 'physical-sky-material :bind
  "set_mie_coefficient" :hash 373806689)
 :void (mie float))

(defgmethod
 (physical-sky-material+get-mie-coefficient :class 'physical-sky-material :bind
  "get_mie_coefficient" :hash 1740695150)
 float)

(defgmethod
 (physical-sky-material+set-mie-eccentricity :class 'physical-sky-material
  :bind "set_mie_eccentricity" :hash 373806689)
 :void (eccentricity float))

(defgmethod
 (physical-sky-material+get-mie-eccentricity :class 'physical-sky-material
  :bind "get_mie_eccentricity" :hash 1740695150)
 float)

(defgmethod
 (physical-sky-material+set-mie-color :class 'physical-sky-material :bind
  "set_mie_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (physical-sky-material+get-mie-color :class 'physical-sky-material :bind
  "get_mie_color" :hash 3444240500)
 color)

(defgmethod
 (physical-sky-material+set-turbidity :class 'physical-sky-material :bind
  "set_turbidity" :hash 373806689)
 :void (turbidity float))

(defgmethod
 (physical-sky-material+get-turbidity :class 'physical-sky-material :bind
  "get_turbidity" :hash 1740695150)
 float)

(defgmethod
 (physical-sky-material+set-sun-disk-scale :class 'physical-sky-material :bind
  "set_sun_disk_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (physical-sky-material+get-sun-disk-scale :class 'physical-sky-material :bind
  "get_sun_disk_scale" :hash 1740695150)
 float)

(defgmethod
 (physical-sky-material+set-ground-color :class 'physical-sky-material :bind
  "set_ground_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (physical-sky-material+get-ground-color :class 'physical-sky-material :bind
  "get_ground_color" :hash 3444240500)
 color)

(defgmethod
 (physical-sky-material+set-energy-multiplier :class 'physical-sky-material
  :bind "set_energy_multiplier" :hash 373806689)
 :void (multiplier float))

(defgmethod
 (physical-sky-material+get-energy-multiplier :class 'physical-sky-material
  :bind "get_energy_multiplier" :hash 1740695150)
 float)

(defgmethod
 (physical-sky-material+set-use-debanding :class 'physical-sky-material :bind
  "set_use_debanding" :hash 2586408642)
 :void (use-debanding bool))

(defgmethod
 (physical-sky-material+get-use-debanding :class 'physical-sky-material :bind
  "get_use_debanding" :hash 36873697)
 bool)

(defgmethod
 (physical-sky-material+set-night-sky :class 'physical-sky-material :bind
  "set_night_sky" :hash 4051416890)
 :void (night-sky texture-2d))

(defgmethod
 (physical-sky-material+get-night-sky :class 'physical-sky-material :bind
  "get_night_sky" :hash 3635182373)
 texture-2d)