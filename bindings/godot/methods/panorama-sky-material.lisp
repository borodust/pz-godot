(common-lisp:in-package :%godot)


(defgmethod
 (panorama-sky-material+set-panorama :class 'panorama-sky-material :bind
  "set_panorama" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (panorama-sky-material+get-panorama :class 'panorama-sky-material :bind
  "get_panorama" :hash 3635182373)
 texture-2d)

(defgmethod
 (panorama-sky-material+set-filtering-enabled :class 'panorama-sky-material
  :bind "set_filtering_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (panorama-sky-material+is-filtering-enabled :class 'panorama-sky-material
  :bind "is_filtering_enabled" :hash 36873697)
 bool)

(defgmethod
 (panorama-sky-material+set-energy-multiplier :class 'panorama-sky-material
  :bind "set_energy_multiplier" :hash 373806689)
 :void (multiplier float))

(defgmethod
 (panorama-sky-material+get-energy-multiplier :class 'panorama-sky-material
  :bind "get_energy_multiplier" :hash 1740695150)
 float)