(common-lisp:in-package :%godot)


(defgproperty ribbon-trail-mesh+shape 'ribbon-trail-mesh :get
 'ribbon-trail-mesh+get-shape :set 'ribbon-trail-mesh+set-shape)

(defgproperty ribbon-trail-mesh+size 'ribbon-trail-mesh :get
 'ribbon-trail-mesh+get-size :set 'ribbon-trail-mesh+set-size)

(defgproperty ribbon-trail-mesh+sections 'ribbon-trail-mesh :get
 'ribbon-trail-mesh+get-sections :set 'ribbon-trail-mesh+set-sections)

(defgproperty ribbon-trail-mesh+section-length 'ribbon-trail-mesh :get
 'ribbon-trail-mesh+get-section-length :set
 'ribbon-trail-mesh+set-section-length)

(defgproperty ribbon-trail-mesh+section-segments 'ribbon-trail-mesh :get
 'ribbon-trail-mesh+get-section-segments :set
 'ribbon-trail-mesh+set-section-segments)

(defgproperty ribbon-trail-mesh+curve 'ribbon-trail-mesh :get
 'ribbon-trail-mesh+get-curve :set 'ribbon-trail-mesh+set-curve)