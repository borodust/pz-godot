(common-lisp:in-package :%godot)


(defgmethod
 (open-xrcomposition-layer-cylinder+set-radius :class
  'open-xrcomposition-layer-cylinder :bind "set_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (open-xrcomposition-layer-cylinder+get-radius :class
  'open-xrcomposition-layer-cylinder :bind "get_radius" :hash 1740695150)
 float)

(defgmethod
 (open-xrcomposition-layer-cylinder+set-aspect-ratio :class
  'open-xrcomposition-layer-cylinder :bind "set_aspect_ratio" :hash 373806689)
 :void (aspect-ratio float))

(defgmethod
 (open-xrcomposition-layer-cylinder+get-aspect-ratio :class
  'open-xrcomposition-layer-cylinder :bind "get_aspect_ratio" :hash 1740695150)
 float)

(defgmethod
 (open-xrcomposition-layer-cylinder+set-central-angle :class
  'open-xrcomposition-layer-cylinder :bind "set_central_angle" :hash 373806689)
 :void (angle float))

(defgmethod
 (open-xrcomposition-layer-cylinder+get-central-angle :class
  'open-xrcomposition-layer-cylinder :bind "get_central_angle" :hash
  1740695150)
 float)

(defgmethod
 (open-xrcomposition-layer-cylinder+set-fallback-segments :class
  'open-xrcomposition-layer-cylinder :bind "set_fallback_segments" :hash
  1286410249)
 :void (segments int))

(defgmethod
 (open-xrcomposition-layer-cylinder+get-fallback-segments :class
  'open-xrcomposition-layer-cylinder :bind "get_fallback_segments" :hash
  3905245786)
 int)