(common-lisp:in-package :%godot)


(defgmethod
 (rdhit-group+set-closest-hit-shader :class 'rdhit-group :bind
  "set_closest_hit_shader" :hash 2556777288)
 :void (p-member rdpipeline-shader))

(defgmethod
 (rdhit-group+get-closest-hit-shader :class 'rdhit-group :bind
  "get_closest_hit_shader" :hash 2937716847)
 rdpipeline-shader)

(defgmethod
 (rdhit-group+set-any-hit-shader :class 'rdhit-group :bind "set_any_hit_shader"
  :hash 2556777288)
 :void (p-member rdpipeline-shader))

(defgmethod
 (rdhit-group+get-any-hit-shader :class 'rdhit-group :bind "get_any_hit_shader"
  :hash 2937716847)
 rdpipeline-shader)

(defgmethod
 (rdhit-group+set-intersection-shader :class 'rdhit-group :bind
  "set_intersection_shader" :hash 2556777288)
 :void (p-member rdpipeline-shader))

(defgmethod
 (rdhit-group+get-intersection-shader :class 'rdhit-group :bind
  "get_intersection_shader" :hash 2937716847)
 rdpipeline-shader)