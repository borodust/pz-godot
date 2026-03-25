(common-lisp:in-package :%godot)


(defgmethod
 (procedural-sky-material+set-sky-top-color :class 'procedural-sky-material
  :bind "set_sky_top_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (procedural-sky-material+get-sky-top-color :class 'procedural-sky-material
  :bind "get_sky_top_color" :hash 3444240500)
 color)

(defgmethod
 (procedural-sky-material+set-sky-horizon-color :class 'procedural-sky-material
  :bind "set_sky_horizon_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (procedural-sky-material+get-sky-horizon-color :class 'procedural-sky-material
  :bind "get_sky_horizon_color" :hash 3444240500)
 color)

(defgmethod
 (procedural-sky-material+set-sky-curve :class 'procedural-sky-material :bind
  "set_sky_curve" :hash 373806689)
 :void (curve float))

(defgmethod
 (procedural-sky-material+get-sky-curve :class 'procedural-sky-material :bind
  "get_sky_curve" :hash 1740695150)
 float)

(defgmethod
 (procedural-sky-material+set-sky-energy-multiplier :class
  'procedural-sky-material :bind "set_sky_energy_multiplier" :hash 373806689)
 :void (multiplier float))

(defgmethod
 (procedural-sky-material+get-sky-energy-multiplier :class
  'procedural-sky-material :bind "get_sky_energy_multiplier" :hash 1740695150)
 float)

(defgmethod
 (procedural-sky-material+set-sky-cover :class 'procedural-sky-material :bind
  "set_sky_cover" :hash 4051416890)
 :void (sky-cover texture-2d))

(defgmethod
 (procedural-sky-material+get-sky-cover :class 'procedural-sky-material :bind
  "get_sky_cover" :hash 3635182373)
 texture-2d)

(defgmethod
 (procedural-sky-material+set-sky-cover-modulate :class
  'procedural-sky-material :bind "set_sky_cover_modulate" :hash 2920490490)
 :void (color color))

(defgmethod
 (procedural-sky-material+get-sky-cover-modulate :class
  'procedural-sky-material :bind "get_sky_cover_modulate" :hash 3444240500)
 color)

(defgmethod
 (procedural-sky-material+set-ground-bottom-color :class
  'procedural-sky-material :bind "set_ground_bottom_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (procedural-sky-material+get-ground-bottom-color :class
  'procedural-sky-material :bind "get_ground_bottom_color" :hash 3444240500)
 color)

(defgmethod
 (procedural-sky-material+set-ground-horizon-color :class
  'procedural-sky-material :bind "set_ground_horizon_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (procedural-sky-material+get-ground-horizon-color :class
  'procedural-sky-material :bind "get_ground_horizon_color" :hash 3444240500)
 color)

(defgmethod
 (procedural-sky-material+set-ground-curve :class 'procedural-sky-material
  :bind "set_ground_curve" :hash 373806689)
 :void (curve float))

(defgmethod
 (procedural-sky-material+get-ground-curve :class 'procedural-sky-material
  :bind "get_ground_curve" :hash 1740695150)
 float)

(defgmethod
 (procedural-sky-material+set-ground-energy-multiplier :class
  'procedural-sky-material :bind "set_ground_energy_multiplier" :hash
  373806689)
 :void (energy float))

(defgmethod
 (procedural-sky-material+get-ground-energy-multiplier :class
  'procedural-sky-material :bind "get_ground_energy_multiplier" :hash
  1740695150)
 float)

(defgmethod
 (procedural-sky-material+set-sun-angle-max :class 'procedural-sky-material
  :bind "set_sun_angle_max" :hash 373806689)
 :void (degrees float))

(defgmethod
 (procedural-sky-material+get-sun-angle-max :class 'procedural-sky-material
  :bind "get_sun_angle_max" :hash 1740695150)
 float)

(defgmethod
 (procedural-sky-material+set-sun-curve :class 'procedural-sky-material :bind
  "set_sun_curve" :hash 373806689)
 :void (curve float))

(defgmethod
 (procedural-sky-material+get-sun-curve :class 'procedural-sky-material :bind
  "get_sun_curve" :hash 1740695150)
 float)

(defgmethod
 (procedural-sky-material+set-use-debanding :class 'procedural-sky-material
  :bind "set_use_debanding" :hash 2586408642)
 :void (use-debanding bool))

(defgmethod
 (procedural-sky-material+get-use-debanding :class 'procedural-sky-material
  :bind "get_use_debanding" :hash 36873697)
 bool)

(defgmethod
 (procedural-sky-material+set-energy-multiplier :class 'procedural-sky-material
  :bind "set_energy_multiplier" :hash 373806689)
 :void (multiplier float))

(defgmethod
 (procedural-sky-material+get-energy-multiplier :class 'procedural-sky-material
  :bind "get_energy_multiplier" :hash 1740695150)
 float)