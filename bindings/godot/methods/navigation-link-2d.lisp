(common-lisp:in-package :%godot)


(defgmethod
 (navigation-link-2d+get-rid :class 'navigation-link-2d :bind "get_rid" :hash
  2944877500)
 rid)

(defgmethod
 (navigation-link-2d+set-enabled :class 'navigation-link-2d :bind "set_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-link-2d+is-enabled :class 'navigation-link-2d :bind "is_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (navigation-link-2d+set-navigation-map :class 'navigation-link-2d :bind
  "set_navigation_map" :hash 2722037293)
 :void (navigation-map rid))

(defgmethod
 (navigation-link-2d+get-navigation-map :class 'navigation-link-2d :bind
  "get_navigation_map" :hash 2944877500)
 rid)

(defgmethod
 (navigation-link-2d+set-bidirectional :class 'navigation-link-2d :bind
  "set_bidirectional" :hash 2586408642)
 :void (bidirectional bool))

(defgmethod
 (navigation-link-2d+is-bidirectional :class 'navigation-link-2d :bind
  "is_bidirectional" :hash 36873697)
 bool)

(defgmethod
 (navigation-link-2d+set-navigation-layers :class 'navigation-link-2d :bind
  "set_navigation_layers" :hash 1286410249)
 :void (navigation-layers int))

(defgmethod
 (navigation-link-2d+get-navigation-layers :class 'navigation-link-2d :bind
  "get_navigation_layers" :hash 3905245786)
 int)

(defgmethod
 (navigation-link-2d+set-navigation-layer-value :class 'navigation-link-2d
  :bind "set_navigation_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (navigation-link-2d+get-navigation-layer-value :class 'navigation-link-2d
  :bind "get_navigation_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (navigation-link-2d+set-start-position :class 'navigation-link-2d :bind
  "set_start_position" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (navigation-link-2d+get-start-position :class 'navigation-link-2d :bind
  "get_start_position" :hash 3341600327)
 vector-2)

(defgmethod
 (navigation-link-2d+set-end-position :class 'navigation-link-2d :bind
  "set_end_position" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (navigation-link-2d+get-end-position :class 'navigation-link-2d :bind
  "get_end_position" :hash 3341600327)
 vector-2)

(defgmethod
 (navigation-link-2d+set-global-start-position :class 'navigation-link-2d :bind
  "set_global_start_position" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (navigation-link-2d+get-global-start-position :class 'navigation-link-2d :bind
  "get_global_start_position" :hash 3341600327)
 vector-2)

(defgmethod
 (navigation-link-2d+set-global-end-position :class 'navigation-link-2d :bind
  "set_global_end_position" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (navigation-link-2d+get-global-end-position :class 'navigation-link-2d :bind
  "get_global_end_position" :hash 3341600327)
 vector-2)

(defgmethod
 (navigation-link-2d+set-enter-cost :class 'navigation-link-2d :bind
  "set_enter_cost" :hash 373806689)
 :void (enter-cost float))

(defgmethod
 (navigation-link-2d+get-enter-cost :class 'navigation-link-2d :bind
  "get_enter_cost" :hash 1740695150)
 float)

(defgmethod
 (navigation-link-2d+set-travel-cost :class 'navigation-link-2d :bind
  "set_travel_cost" :hash 373806689)
 :void (travel-cost float))

(defgmethod
 (navigation-link-2d+get-travel-cost :class 'navigation-link-2d :bind
  "get_travel_cost" :hash 1740695150)
 float)