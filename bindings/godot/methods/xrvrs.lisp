(common-lisp:in-package :%godot)


(defgmethod
 (xrvrs+get-vrs-min-radius :class 'xrvrs :bind "get_vrs_min_radius" :hash
  1740695150)
 float)

(defgmethod
 (xrvrs+set-vrs-min-radius :class 'xrvrs :bind "set_vrs_min_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (xrvrs+get-vrs-strength :class 'xrvrs :bind "get_vrs_strength" :hash
  1740695150)
 float)

(defgmethod
 (xrvrs+set-vrs-strength :class 'xrvrs :bind "set_vrs_strength" :hash
  373806689)
 :void (strength float))

(defgmethod
 (xrvrs+get-vrs-render-region :class 'xrvrs :bind "get_vrs_render_region" :hash
  410525958)
 rect-2i)

(defgmethod
 (xrvrs+set-vrs-render-region :class 'xrvrs :bind "set_vrs_render_region" :hash
  1763793166)
 :void (render-region rect-2i))

(defgmethod
 (xrvrs+make-vrs-texture :class 'xrvrs :bind "make_vrs_texture" :hash
  3647044786)
 rid (target-size vector-2) (eye-foci packed-vector-2array))