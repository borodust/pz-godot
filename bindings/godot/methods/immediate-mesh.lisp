(common-lisp:in-package :%godot)


(defgmethod
 (immediate-mesh+surface-begin :class 'immediate-mesh :bind "surface_begin"
  :hash 2794442543)
 :void (primitive mesh+primitive-type) (material material))

(defgmethod
 (immediate-mesh+surface-set-color :class 'immediate-mesh :bind
  "surface_set_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (immediate-mesh+surface-set-normal :class 'immediate-mesh :bind
  "surface_set_normal" :hash 3460891852)
 :void (normal vector-3))

(defgmethod
 (immediate-mesh+surface-set-tangent :class 'immediate-mesh :bind
  "surface_set_tangent" :hash 3505987427)
 :void (tangent plane))

(defgmethod
 (immediate-mesh+surface-set-uv :class 'immediate-mesh :bind "surface_set_uv"
  :hash 743155724)
 :void (uv vector-2))

(defgmethod
 (immediate-mesh+surface-set-uv2 :class 'immediate-mesh :bind "surface_set_uv2"
  :hash 743155724)
 :void (uv2 vector-2))

(defgmethod
 (immediate-mesh+surface-add-vertex :class 'immediate-mesh :bind
  "surface_add_vertex" :hash 3460891852)
 :void (vertex vector-3))

(defgmethod
 (immediate-mesh+surface-add-vertex-2d :class 'immediate-mesh :bind
  "surface_add_vertex_2d" :hash 743155724)
 :void (vertex vector-2))

(defgmethod
 (immediate-mesh+surface-end :class 'immediate-mesh :bind "surface_end" :hash
  3218959716)
 :void)

(defgmethod
 (immediate-mesh+clear-surfaces :class 'immediate-mesh :bind "clear_surfaces"
  :hash 3218959716)
 :void)