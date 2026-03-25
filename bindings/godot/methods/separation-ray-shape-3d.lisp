(common-lisp:in-package :%godot)


(defgmethod
 (separation-ray-shape-3d+set-length :class 'separation-ray-shape-3d :bind
  "set_length" :hash 373806689)
 :void (length float))

(defgmethod
 (separation-ray-shape-3d+get-length :class 'separation-ray-shape-3d :bind
  "get_length" :hash 1740695150)
 float)

(defgmethod
 (separation-ray-shape-3d+set-slide-on-slope :class 'separation-ray-shape-3d
  :bind "set_slide_on_slope" :hash 2586408642)
 :void (active bool))

(defgmethod
 (separation-ray-shape-3d+get-slide-on-slope :class 'separation-ray-shape-3d
  :bind "get_slide_on_slope" :hash 36873697)
 bool)