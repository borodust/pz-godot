(common-lisp:in-package :%godot)


(defgmethod
 (navigation-obstacle-3d+get-rid :class 'navigation-obstacle-3d :bind "get_rid"
  :hash 2944877500)
 rid)

(defgmethod
 (navigation-obstacle-3d+set-avoidance-enabled :class 'navigation-obstacle-3d
  :bind "set_avoidance_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-obstacle-3d+get-avoidance-enabled :class 'navigation-obstacle-3d
  :bind "get_avoidance_enabled" :hash 36873697)
 bool)

(defgmethod
 (navigation-obstacle-3d+set-navigation-map :class 'navigation-obstacle-3d
  :bind "set_navigation_map" :hash 2722037293)
 :void (navigation-map rid))

(defgmethod
 (navigation-obstacle-3d+get-navigation-map :class 'navigation-obstacle-3d
  :bind "get_navigation_map" :hash 2944877500)
 rid)

(defgmethod
 (navigation-obstacle-3d+set-radius :class 'navigation-obstacle-3d :bind
  "set_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (navigation-obstacle-3d+get-radius :class 'navigation-obstacle-3d :bind
  "get_radius" :hash 1740695150)
 float)

(defgmethod
 (navigation-obstacle-3d+set-height :class 'navigation-obstacle-3d :bind
  "set_height" :hash 373806689)
 :void (height float))

(defgmethod
 (navigation-obstacle-3d+get-height :class 'navigation-obstacle-3d :bind
  "get_height" :hash 1740695150)
 float)

(defgmethod
 (navigation-obstacle-3d+set-velocity :class 'navigation-obstacle-3d :bind
  "set_velocity" :hash 3460891852)
 :void (velocity vector-3))

(defgmethod
 (navigation-obstacle-3d+get-velocity :class 'navigation-obstacle-3d :bind
  "get_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (navigation-obstacle-3d+set-vertices :class 'navigation-obstacle-3d :bind
  "set_vertices" :hash 334873810)
 :void (vertices packed-vector-3array))

(defgmethod
 (navigation-obstacle-3d+get-vertices :class 'navigation-obstacle-3d :bind
  "get_vertices" :hash 497664490)
 packed-vector-3array)

(defgmethod
 (navigation-obstacle-3d+set-avoidance-layers :class 'navigation-obstacle-3d
  :bind "set_avoidance_layers" :hash 1286410249)
 :void (layers int))

(defgmethod
 (navigation-obstacle-3d+get-avoidance-layers :class 'navigation-obstacle-3d
  :bind "get_avoidance_layers" :hash 3905245786)
 int)

(defgmethod
 (navigation-obstacle-3d+set-avoidance-layer-value :class
  'navigation-obstacle-3d :bind "set_avoidance_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (navigation-obstacle-3d+get-avoidance-layer-value :class
  'navigation-obstacle-3d :bind "get_avoidance_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (navigation-obstacle-3d+set-use-3d-avoidance :class 'navigation-obstacle-3d
  :bind "set_use_3d_avoidance" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-obstacle-3d+get-use-3d-avoidance :class 'navigation-obstacle-3d
  :bind "get_use_3d_avoidance" :hash 36873697)
 bool)

(defgmethod
 (navigation-obstacle-3d+set-affect-navigation-mesh :class
  'navigation-obstacle-3d :bind "set_affect_navigation_mesh" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-obstacle-3d+get-affect-navigation-mesh :class
  'navigation-obstacle-3d :bind "get_affect_navigation_mesh" :hash 36873697)
 bool)

(defgmethod
 (navigation-obstacle-3d+set-carve-navigation-mesh :class
  'navigation-obstacle-3d :bind "set_carve_navigation_mesh" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-obstacle-3d+get-carve-navigation-mesh :class
  'navigation-obstacle-3d :bind "get_carve_navigation_mesh" :hash 36873697)
 bool)