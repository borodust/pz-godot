(common-lisp:in-package :%godot)


(defgmethod
 (ribbon-trail-mesh+set-size :class 'ribbon-trail-mesh :bind "set_size" :hash
  373806689)
 :void (size float))

(defgmethod
 (ribbon-trail-mesh+get-size :class 'ribbon-trail-mesh :bind "get_size" :hash
  1740695150)
 float)

(defgmethod
 (ribbon-trail-mesh+set-sections :class 'ribbon-trail-mesh :bind "set_sections"
  :hash 1286410249)
 :void (sections int))

(defgmethod
 (ribbon-trail-mesh+get-sections :class 'ribbon-trail-mesh :bind "get_sections"
  :hash 3905245786)
 int)

(defgmethod
 (ribbon-trail-mesh+set-section-length :class 'ribbon-trail-mesh :bind
  "set_section_length" :hash 373806689)
 :void (section-length float))

(defgmethod
 (ribbon-trail-mesh+get-section-length :class 'ribbon-trail-mesh :bind
  "get_section_length" :hash 1740695150)
 float)

(defgmethod
 (ribbon-trail-mesh+set-section-segments :class 'ribbon-trail-mesh :bind
  "set_section_segments" :hash 1286410249)
 :void (section-segments int))

(defgmethod
 (ribbon-trail-mesh+get-section-segments :class 'ribbon-trail-mesh :bind
  "get_section_segments" :hash 3905245786)
 int)

(defgmethod
 (ribbon-trail-mesh+set-curve :class 'ribbon-trail-mesh :bind "set_curve" :hash
  270443179)
 :void (curve curve))

(defgmethod
 (ribbon-trail-mesh+get-curve :class 'ribbon-trail-mesh :bind "get_curve" :hash
  2460114913)
 curve)

(defgmethod
 (ribbon-trail-mesh+set-shape :class 'ribbon-trail-mesh :bind "set_shape" :hash
  1684440262)
 :void (shape ribbon-trail-mesh+shape))

(defgmethod
 (ribbon-trail-mesh+get-shape :class 'ribbon-trail-mesh :bind "get_shape" :hash
  1317484155)
 ribbon-trail-mesh+shape)