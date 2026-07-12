(common-lisp:in-package :%godot)


(defgmethod
 (area-light-3d+set-area-texture :class 'area-light-3d :bind "set_area_texture"
  :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (area-light-3d+get-area-texture :class 'area-light-3d :bind "get_area_texture"
  :hash 3635182373)
 texture-2d)

(defgmethod
 (area-light-3d+set-area-size :class 'area-light-3d :bind "set_area_size" :hash
  743155724)
 :void (area-size vector-2))

(defgmethod
 (area-light-3d+get-area-size :class 'area-light-3d :bind "get_area_size" :hash
  3341600327)
 vector-2)

(defgmethod
 (area-light-3d+set-area-normalize-energy :class 'area-light-3d :bind
  "set_area_normalize_energy" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (area-light-3d+is-area-normalizing-energy :class 'area-light-3d :bind
  "is_area_normalizing_energy" :hash 36873697)
 bool)