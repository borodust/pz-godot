(common-lisp:in-package :%godot)


(defgmethod
 (fog-material+set-density :class 'fog-material :bind "set_density" :hash
  373806689)
 :void (density float))

(defgmethod
 (fog-material+get-density :class 'fog-material :bind "get_density" :hash
  1740695150)
 float)

(defgmethod
 (fog-material+set-albedo :class 'fog-material :bind "set_albedo" :hash
  2920490490)
 :void (albedo color))

(defgmethod
 (fog-material+get-albedo :class 'fog-material :bind "get_albedo" :hash
  3444240500)
 color)

(defgmethod
 (fog-material+set-emission :class 'fog-material :bind "set_emission" :hash
  2920490490)
 :void (emission color))

(defgmethod
 (fog-material+get-emission :class 'fog-material :bind "get_emission" :hash
  3444240500)
 color)

(defgmethod
 (fog-material+set-height-falloff :class 'fog-material :bind
  "set_height_falloff" :hash 373806689)
 :void (height-falloff float))

(defgmethod
 (fog-material+get-height-falloff :class 'fog-material :bind
  "get_height_falloff" :hash 1740695150)
 float)

(defgmethod
 (fog-material+set-edge-fade :class 'fog-material :bind "set_edge_fade" :hash
  373806689)
 :void (edge-fade float))

(defgmethod
 (fog-material+get-edge-fade :class 'fog-material :bind "get_edge_fade" :hash
  1740695150)
 float)

(defgmethod
 (fog-material+set-density-texture :class 'fog-material :bind
  "set_density_texture" :hash 1188404210)
 :void (density-texture texture-3d))

(defgmethod
 (fog-material+get-density-texture :class 'fog-material :bind
  "get_density_texture" :hash 373985333)
 texture-3d)