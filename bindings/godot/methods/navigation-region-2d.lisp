(common-lisp:in-package :%godot)


(defgmethod
 (navigation-region-2d+get-rid :class 'navigation-region-2d :bind "get_rid"
  :hash 2944877500)
 rid)

(defgmethod
 (navigation-region-2d+set-navigation-polygon :class 'navigation-region-2d
  :bind "set_navigation_polygon" :hash 1515040758)
 :void (navigation-polygon navigation-polygon))

(defgmethod
 (navigation-region-2d+get-navigation-polygon :class 'navigation-region-2d
  :bind "get_navigation_polygon" :hash 1046532237)
 navigation-polygon)

(defgmethod
 (navigation-region-2d+set-enabled :class 'navigation-region-2d :bind
  "set_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-region-2d+is-enabled :class 'navigation-region-2d :bind
  "is_enabled" :hash 36873697)
 bool)

(defgmethod
 (navigation-region-2d+set-navigation-map :class 'navigation-region-2d :bind
  "set_navigation_map" :hash 2722037293)
 :void (navigation-map rid))

(defgmethod
 (navigation-region-2d+get-navigation-map :class 'navigation-region-2d :bind
  "get_navigation_map" :hash 2944877500)
 rid)

(defgmethod
 (navigation-region-2d+set-use-edge-connections :class 'navigation-region-2d
  :bind "set_use_edge_connections" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-region-2d+get-use-edge-connections :class 'navigation-region-2d
  :bind "get_use_edge_connections" :hash 36873697)
 bool)

(defgmethod
 (navigation-region-2d+set-navigation-layers :class 'navigation-region-2d :bind
  "set_navigation_layers" :hash 1286410249)
 :void (navigation-layers int))

(defgmethod
 (navigation-region-2d+get-navigation-layers :class 'navigation-region-2d :bind
  "get_navigation_layers" :hash 3905245786)
 int)

(defgmethod
 (navigation-region-2d+set-navigation-layer-value :class 'navigation-region-2d
  :bind "set_navigation_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (navigation-region-2d+get-navigation-layer-value :class 'navigation-region-2d
  :bind "get_navigation_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (navigation-region-2d+get-region-rid :class 'navigation-region-2d :bind
  "get_region_rid" :hash 2944877500)
 rid)

(defgmethod
 (navigation-region-2d+set-enter-cost :class 'navigation-region-2d :bind
  "set_enter_cost" :hash 373806689)
 :void (enter-cost float))

(defgmethod
 (navigation-region-2d+get-enter-cost :class 'navigation-region-2d :bind
  "get_enter_cost" :hash 1740695150)
 float)

(defgmethod
 (navigation-region-2d+set-travel-cost :class 'navigation-region-2d :bind
  "set_travel_cost" :hash 373806689)
 :void (travel-cost float))

(defgmethod
 (navigation-region-2d+get-travel-cost :class 'navigation-region-2d :bind
  "get_travel_cost" :hash 1740695150)
 float)

(defgmethod
 (navigation-region-2d+bake-navigation-polygon :class 'navigation-region-2d
  :bind "bake_navigation_polygon" :hash 3216645846)
 :void (on-thread bool))

(defgmethod
 (navigation-region-2d+is-baking :class 'navigation-region-2d :bind "is_baking"
  :hash 36873697)
 bool)

(defgmethod
 (navigation-region-2d+get-bounds :class 'navigation-region-2d :bind
  "get_bounds" :hash 1639390495)
 rect-2)