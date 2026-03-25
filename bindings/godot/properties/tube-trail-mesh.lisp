(common-lisp:in-package :%godot)


(defgproperty tube-trail-mesh+radius 'tube-trail-mesh :get
 'tube-trail-mesh+get-radius :set 'tube-trail-mesh+set-radius)

(defgproperty tube-trail-mesh+radial-steps 'tube-trail-mesh :get
 'tube-trail-mesh+get-radial-steps :set 'tube-trail-mesh+set-radial-steps)

(defgproperty tube-trail-mesh+sections 'tube-trail-mesh :get
 'tube-trail-mesh+get-sections :set 'tube-trail-mesh+set-sections)

(defgproperty tube-trail-mesh+section-length 'tube-trail-mesh :get
 'tube-trail-mesh+get-section-length :set 'tube-trail-mesh+set-section-length)

(defgproperty tube-trail-mesh+section-rings 'tube-trail-mesh :get
 'tube-trail-mesh+get-section-rings :set 'tube-trail-mesh+set-section-rings)

(defgproperty tube-trail-mesh+cap-top 'tube-trail-mesh :get
 'tube-trail-mesh+is-cap-top :set 'tube-trail-mesh+set-cap-top)

(defgproperty tube-trail-mesh+cap-bottom 'tube-trail-mesh :get
 'tube-trail-mesh+is-cap-bottom :set 'tube-trail-mesh+set-cap-bottom)

(defgproperty tube-trail-mesh+curve 'tube-trail-mesh :get
 'tube-trail-mesh+get-curve :set 'tube-trail-mesh+set-curve)