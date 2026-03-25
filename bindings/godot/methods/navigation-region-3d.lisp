(common-lisp:in-package :%godot)


(defgmethod
 (navigation-region-3d+get-rid :class 'navigation-region-3d :bind "get_rid"
  :hash 2944877500)
 rid)

(defgmethod
 (navigation-region-3d+set-navigation-mesh :class 'navigation-region-3d :bind
  "set_navigation_mesh" :hash 2923361153)
 :void (navigation-mesh navigation-mesh))

(defgmethod
 (navigation-region-3d+get-navigation-mesh :class 'navigation-region-3d :bind
  "get_navigation_mesh" :hash 1468720886)
 navigation-mesh)

(defgmethod
 (navigation-region-3d+set-enabled :class 'navigation-region-3d :bind
  "set_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-region-3d+is-enabled :class 'navigation-region-3d :bind
  "is_enabled" :hash 36873697)
 bool)

(defgmethod
 (navigation-region-3d+set-navigation-map :class 'navigation-region-3d :bind
  "set_navigation_map" :hash 2722037293)
 :void (navigation-map rid))

(defgmethod
 (navigation-region-3d+get-navigation-map :class 'navigation-region-3d :bind
  "get_navigation_map" :hash 2944877500)
 rid)

(defgmethod
 (navigation-region-3d+set-use-edge-connections :class 'navigation-region-3d
  :bind "set_use_edge_connections" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-region-3d+get-use-edge-connections :class 'navigation-region-3d
  :bind "get_use_edge_connections" :hash 36873697)
 bool)

(defgmethod
 (navigation-region-3d+set-navigation-layers :class 'navigation-region-3d :bind
  "set_navigation_layers" :hash 1286410249)
 :void (navigation-layers int))

(defgmethod
 (navigation-region-3d+get-navigation-layers :class 'navigation-region-3d :bind
  "get_navigation_layers" :hash 3905245786)
 int)

(defgmethod
 (navigation-region-3d+set-navigation-layer-value :class 'navigation-region-3d
  :bind "set_navigation_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (navigation-region-3d+get-navigation-layer-value :class 'navigation-region-3d
  :bind "get_navigation_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (navigation-region-3d+get-region-rid :class 'navigation-region-3d :bind
  "get_region_rid" :hash 2944877500)
 rid)

(defgmethod
 (navigation-region-3d+set-enter-cost :class 'navigation-region-3d :bind
  "set_enter_cost" :hash 373806689)
 :void (enter-cost float))

(defgmethod
 (navigation-region-3d+get-enter-cost :class 'navigation-region-3d :bind
  "get_enter_cost" :hash 1740695150)
 float)

(defgmethod
 (navigation-region-3d+set-travel-cost :class 'navigation-region-3d :bind
  "set_travel_cost" :hash 373806689)
 :void (travel-cost float))

(defgmethod
 (navigation-region-3d+get-travel-cost :class 'navigation-region-3d :bind
  "get_travel_cost" :hash 1740695150)
 float)

(defgmethod
 (navigation-region-3d+bake-navigation-mesh :class 'navigation-region-3d :bind
  "bake_navigation_mesh" :hash 3216645846)
 :void (on-thread bool))

(defgmethod
 (navigation-region-3d+is-baking :class 'navigation-region-3d :bind "is_baking"
  :hash 36873697)
 bool)

(defgmethod
 (navigation-region-3d+get-bounds :class 'navigation-region-3d :bind
  "get_bounds" :hash 1068685055)
 aabb)