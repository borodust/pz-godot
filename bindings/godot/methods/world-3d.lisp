(common-lisp:in-package :%godot)


(defgmethod
 (world-3d+get-space :class 'world-3d :bind "get_space" :hash 2944877500) rid)

(defgmethod
 (world-3d+get-navigation-map :class 'world-3d :bind "get_navigation_map" :hash
  2944877500)
 rid)

(defgmethod
 (world-3d+get-scenario :class 'world-3d :bind "get_scenario" :hash 2944877500)
 rid)

(defgmethod
 (world-3d+set-environment :class 'world-3d :bind "set_environment" :hash
  4143518816)
 :void (env environment))

(defgmethod
 (world-3d+get-environment :class 'world-3d :bind "get_environment" :hash
  3082064660)
 environment)

(defgmethod
 (world-3d+set-fallback-environment :class 'world-3d :bind
  "set_fallback_environment" :hash 4143518816)
 :void (env environment))

(defgmethod
 (world-3d+get-fallback-environment :class 'world-3d :bind
  "get_fallback_environment" :hash 3082064660)
 environment)

(defgmethod
 (world-3d+set-camera-attributes :class 'world-3d :bind "set_camera_attributes"
  :hash 2817810567)
 :void (attributes camera-attributes))

(defgmethod
 (world-3d+get-camera-attributes :class 'world-3d :bind "get_camera_attributes"
  :hash 3921283215)
 camera-attributes)

(defgmethod
 (world-3d+get-direct-space-state :class 'world-3d :bind
  "get_direct_space_state" :hash 2069328350)
 physics-direct-space-state-3d)