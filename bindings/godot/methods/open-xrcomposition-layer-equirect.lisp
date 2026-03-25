(common-lisp:in-package :%godot)


(defgmethod
 (open-xrcomposition-layer-equirect+set-radius :class
  'open-xrcomposition-layer-equirect :bind "set_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (open-xrcomposition-layer-equirect+get-radius :class
  'open-xrcomposition-layer-equirect :bind "get_radius" :hash 1740695150)
 float)

(defgmethod
 (open-xrcomposition-layer-equirect+set-central-horizontal-angle :class
  'open-xrcomposition-layer-equirect :bind "set_central_horizontal_angle" :hash
  373806689)
 :void (angle float))

(defgmethod
 (open-xrcomposition-layer-equirect+get-central-horizontal-angle :class
  'open-xrcomposition-layer-equirect :bind "get_central_horizontal_angle" :hash
  1740695150)
 float)

(defgmethod
 (open-xrcomposition-layer-equirect+set-upper-vertical-angle :class
  'open-xrcomposition-layer-equirect :bind "set_upper_vertical_angle" :hash
  373806689)
 :void (angle float))

(defgmethod
 (open-xrcomposition-layer-equirect+get-upper-vertical-angle :class
  'open-xrcomposition-layer-equirect :bind "get_upper_vertical_angle" :hash
  1740695150)
 float)

(defgmethod
 (open-xrcomposition-layer-equirect+set-lower-vertical-angle :class
  'open-xrcomposition-layer-equirect :bind "set_lower_vertical_angle" :hash
  373806689)
 :void (angle float))

(defgmethod
 (open-xrcomposition-layer-equirect+get-lower-vertical-angle :class
  'open-xrcomposition-layer-equirect :bind "get_lower_vertical_angle" :hash
  1740695150)
 float)

(defgmethod
 (open-xrcomposition-layer-equirect+set-fallback-segments :class
  'open-xrcomposition-layer-equirect :bind "set_fallback_segments" :hash
  1286410249)
 :void (segments int))

(defgmethod
 (open-xrcomposition-layer-equirect+get-fallback-segments :class
  'open-xrcomposition-layer-equirect :bind "get_fallback_segments" :hash
  3905245786)
 int)