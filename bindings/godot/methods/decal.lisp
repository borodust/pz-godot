(common-lisp:in-package :%godot)


(defgmethod (decal+set-size :class 'decal :bind "set_size" :hash 3460891852)
 :void (size vector-3))

(defgmethod (decal+get-size :class 'decal :bind "get_size" :hash 3360562783)
 vector-3)

(defgmethod
 (decal+set-texture :class 'decal :bind "set_texture" :hash 2086764391) :void
 (type decal+decal-texture) (texture texture-2d))

(defgmethod
 (decal+get-texture :class 'decal :bind "get_texture" :hash 3244119503)
 texture-2d (type decal+decal-texture))

(defgmethod
 (decal+set-emission-energy :class 'decal :bind "set_emission_energy" :hash
  373806689)
 :void (energy float))

(defgmethod
 (decal+get-emission-energy :class 'decal :bind "get_emission_energy" :hash
  1740695150)
 float)

(defgmethod
 (decal+set-albedo-mix :class 'decal :bind "set_albedo_mix" :hash 373806689)
 :void (energy float))

(defgmethod
 (decal+get-albedo-mix :class 'decal :bind "get_albedo_mix" :hash 1740695150)
 float)

(defgmethod
 (decal+set-modulate :class 'decal :bind "set_modulate" :hash 2920490490) :void
 (color color))

(defgmethod
 (decal+get-modulate :class 'decal :bind "get_modulate" :hash 3444240500) color)

(defgmethod
 (decal+set-upper-fade :class 'decal :bind "set_upper_fade" :hash 373806689)
 :void (fade float))

(defgmethod
 (decal+get-upper-fade :class 'decal :bind "get_upper_fade" :hash 1740695150)
 float)

(defgmethod
 (decal+set-lower-fade :class 'decal :bind "set_lower_fade" :hash 373806689)
 :void (fade float))

(defgmethod
 (decal+get-lower-fade :class 'decal :bind "get_lower_fade" :hash 1740695150)
 float)

(defgmethod
 (decal+set-normal-fade :class 'decal :bind "set_normal_fade" :hash 373806689)
 :void (fade float))

(defgmethod
 (decal+get-normal-fade :class 'decal :bind "get_normal_fade" :hash 1740695150)
 float)

(defgmethod
 (decal+set-enable-distance-fade :class 'decal :bind "set_enable_distance_fade"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (decal+is-distance-fade-enabled :class 'decal :bind "is_distance_fade_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (decal+set-distance-fade-begin :class 'decal :bind "set_distance_fade_begin"
  :hash 373806689)
 :void (distance float))

(defgmethod
 (decal+get-distance-fade-begin :class 'decal :bind "get_distance_fade_begin"
  :hash 1740695150)
 float)

(defgmethod
 (decal+set-distance-fade-length :class 'decal :bind "set_distance_fade_length"
  :hash 373806689)
 :void (distance float))

(defgmethod
 (decal+get-distance-fade-length :class 'decal :bind "get_distance_fade_length"
  :hash 1740695150)
 float)

(defgmethod
 (decal+set-cull-mask :class 'decal :bind "set_cull_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (decal+get-cull-mask :class 'decal :bind "get_cull_mask" :hash 3905245786) int)