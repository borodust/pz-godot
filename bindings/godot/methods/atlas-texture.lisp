(common-lisp:in-package :%godot)


(defgmethod
 (atlas-texture+set-atlas :class 'atlas-texture :bind "set_atlas" :hash
  4051416890)
 :void (atlas texture-2d))

(defgmethod
 (atlas-texture+get-atlas :class 'atlas-texture :bind "get_atlas" :hash
  3635182373)
 texture-2d)

(defgmethod
 (atlas-texture+set-region :class 'atlas-texture :bind "set_region" :hash
  2046264180)
 :void (region rect-2))

(defgmethod
 (atlas-texture+get-region :class 'atlas-texture :bind "get_region" :hash
  1639390495)
 rect-2)

(defgmethod
 (atlas-texture+set-margin :class 'atlas-texture :bind "set_margin" :hash
  2046264180)
 :void (margin rect-2))

(defgmethod
 (atlas-texture+get-margin :class 'atlas-texture :bind "get_margin" :hash
  1639390495)
 rect-2)

(defgmethod
 (atlas-texture+set-filter-clip :class 'atlas-texture :bind "set_filter_clip"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (atlas-texture+has-filter-clip :class 'atlas-texture :bind "has_filter_clip"
  :hash 36873697)
 bool)