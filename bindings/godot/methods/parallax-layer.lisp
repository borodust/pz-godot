(common-lisp:in-package :%godot)


(defgmethod
 (parallax-layer+set-motion-scale :class 'parallax-layer :bind
  "set_motion_scale" :hash 743155724)
 :void (scale vector-2))

(defgmethod
 (parallax-layer+get-motion-scale :class 'parallax-layer :bind
  "get_motion_scale" :hash 3341600327)
 vector-2)

(defgmethod
 (parallax-layer+set-motion-offset :class 'parallax-layer :bind
  "set_motion_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (parallax-layer+get-motion-offset :class 'parallax-layer :bind
  "get_motion_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (parallax-layer+set-mirroring :class 'parallax-layer :bind "set_mirroring"
  :hash 743155724)
 :void (mirror vector-2))

(defgmethod
 (parallax-layer+get-mirroring :class 'parallax-layer :bind "get_mirroring"
  :hash 3341600327)
 vector-2)