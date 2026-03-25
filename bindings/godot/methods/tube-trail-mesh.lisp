(common-lisp:in-package :%godot)


(defgmethod
 (tube-trail-mesh+set-radius :class 'tube-trail-mesh :bind "set_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (tube-trail-mesh+get-radius :class 'tube-trail-mesh :bind "get_radius" :hash
  1740695150)
 float)

(defgmethod
 (tube-trail-mesh+set-radial-steps :class 'tube-trail-mesh :bind
  "set_radial_steps" :hash 1286410249)
 :void (radial-steps int))

(defgmethod
 (tube-trail-mesh+get-radial-steps :class 'tube-trail-mesh :bind
  "get_radial_steps" :hash 3905245786)
 int)

(defgmethod
 (tube-trail-mesh+set-sections :class 'tube-trail-mesh :bind "set_sections"
  :hash 1286410249)
 :void (sections int))

(defgmethod
 (tube-trail-mesh+get-sections :class 'tube-trail-mesh :bind "get_sections"
  :hash 3905245786)
 int)

(defgmethod
 (tube-trail-mesh+set-section-length :class 'tube-trail-mesh :bind
  "set_section_length" :hash 373806689)
 :void (section-length float))

(defgmethod
 (tube-trail-mesh+get-section-length :class 'tube-trail-mesh :bind
  "get_section_length" :hash 1740695150)
 float)

(defgmethod
 (tube-trail-mesh+set-section-rings :class 'tube-trail-mesh :bind
  "set_section_rings" :hash 1286410249)
 :void (section-rings int))

(defgmethod
 (tube-trail-mesh+get-section-rings :class 'tube-trail-mesh :bind
  "get_section_rings" :hash 3905245786)
 int)

(defgmethod
 (tube-trail-mesh+set-cap-top :class 'tube-trail-mesh :bind "set_cap_top" :hash
  2586408642)
 :void (cap-top bool))

(defgmethod
 (tube-trail-mesh+is-cap-top :class 'tube-trail-mesh :bind "is_cap_top" :hash
  36873697)
 bool)

(defgmethod
 (tube-trail-mesh+set-cap-bottom :class 'tube-trail-mesh :bind "set_cap_bottom"
  :hash 2586408642)
 :void (cap-bottom bool))

(defgmethod
 (tube-trail-mesh+is-cap-bottom :class 'tube-trail-mesh :bind "is_cap_bottom"
  :hash 36873697)
 bool)

(defgmethod
 (tube-trail-mesh+set-curve :class 'tube-trail-mesh :bind "set_curve" :hash
  270443179)
 :void (curve curve))

(defgmethod
 (tube-trail-mesh+get-curve :class 'tube-trail-mesh :bind "get_curve" :hash
  2460114913)
 curve)