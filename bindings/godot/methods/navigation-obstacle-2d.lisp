(common-lisp:in-package :%godot)


(defgmethod
 (navigation-obstacle-2d+get-rid :class 'navigation-obstacle-2d :bind "get_rid"
  :hash 2944877500)
 rid)

(defgmethod
 (navigation-obstacle-2d+set-avoidance-enabled :class 'navigation-obstacle-2d
  :bind "set_avoidance_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-obstacle-2d+get-avoidance-enabled :class 'navigation-obstacle-2d
  :bind "get_avoidance_enabled" :hash 36873697)
 bool)

(defgmethod
 (navigation-obstacle-2d+set-navigation-map :class 'navigation-obstacle-2d
  :bind "set_navigation_map" :hash 2722037293)
 :void (navigation-map rid))

(defgmethod
 (navigation-obstacle-2d+get-navigation-map :class 'navigation-obstacle-2d
  :bind "get_navigation_map" :hash 2944877500)
 rid)

(defgmethod
 (navigation-obstacle-2d+set-radius :class 'navigation-obstacle-2d :bind
  "set_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (navigation-obstacle-2d+get-radius :class 'navigation-obstacle-2d :bind
  "get_radius" :hash 1740695150)
 float)

(defgmethod
 (navigation-obstacle-2d+set-velocity :class 'navigation-obstacle-2d :bind
  "set_velocity" :hash 743155724)
 :void (velocity vector-2))

(defgmethod
 (navigation-obstacle-2d+get-velocity :class 'navigation-obstacle-2d :bind
  "get_velocity" :hash 3341600327)
 vector-2)

(defgmethod
 (navigation-obstacle-2d+set-vertices :class 'navigation-obstacle-2d :bind
  "set_vertices" :hash 1509147220)
 :void (vertices packed-vector-2array))

(defgmethod
 (navigation-obstacle-2d+get-vertices :class 'navigation-obstacle-2d :bind
  "get_vertices" :hash 2961356807)
 packed-vector-2array)

(defgmethod
 (navigation-obstacle-2d+set-avoidance-layers :class 'navigation-obstacle-2d
  :bind "set_avoidance_layers" :hash 1286410249)
 :void (layers int))

(defgmethod
 (navigation-obstacle-2d+get-avoidance-layers :class 'navigation-obstacle-2d
  :bind "get_avoidance_layers" :hash 3905245786)
 int)

(defgmethod
 (navigation-obstacle-2d+set-avoidance-layer-value :class
  'navigation-obstacle-2d :bind "set_avoidance_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (navigation-obstacle-2d+get-avoidance-layer-value :class
  'navigation-obstacle-2d :bind "get_avoidance_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (navigation-obstacle-2d+set-affect-navigation-mesh :class
  'navigation-obstacle-2d :bind "set_affect_navigation_mesh" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-obstacle-2d+get-affect-navigation-mesh :class
  'navigation-obstacle-2d :bind "get_affect_navigation_mesh" :hash 36873697)
 bool)

(defgmethod
 (navigation-obstacle-2d+set-carve-navigation-mesh :class
  'navigation-obstacle-2d :bind "set_carve_navigation_mesh" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-obstacle-2d+get-carve-navigation-mesh :class
  'navigation-obstacle-2d :bind "get_carve_navigation_mesh" :hash 36873697)
 bool)