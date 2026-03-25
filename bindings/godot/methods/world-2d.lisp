(common-lisp:in-package :%godot)


(defgmethod
 (world-2d+get-canvas :class 'world-2d :bind "get_canvas" :hash 2944877500) rid)

(defgmethod
 (world-2d+get-navigation-map :class 'world-2d :bind "get_navigation_map" :hash
  2944877500)
 rid)

(defgmethod
 (world-2d+get-space :class 'world-2d :bind "get_space" :hash 2944877500) rid)

(defgmethod
 (world-2d+get-direct-space-state :class 'world-2d :bind
  "get_direct_space_state" :hash 2506717822)
 physics-direct-space-state-2d)