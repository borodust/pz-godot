(common-lisp:in-package :%godot)


(defgproperty navigation-agent-2d+target-position 'navigation-agent-2d :get
 'navigation-agent-2d+get-target-position :set
 'navigation-agent-2d+set-target-position)

(defgproperty navigation-agent-2d+path-desired-distance 'navigation-agent-2d
 :get 'navigation-agent-2d+get-path-desired-distance :set
 'navigation-agent-2d+set-path-desired-distance)

(defgproperty navigation-agent-2d+target-desired-distance 'navigation-agent-2d
 :get 'navigation-agent-2d+get-target-desired-distance :set
 'navigation-agent-2d+set-target-desired-distance)

(defgproperty navigation-agent-2d+path-max-distance 'navigation-agent-2d :get
 'navigation-agent-2d+get-path-max-distance :set
 'navigation-agent-2d+set-path-max-distance)

(defgproperty navigation-agent-2d+navigation-layers 'navigation-agent-2d :get
 'navigation-agent-2d+get-navigation-layers :set
 'navigation-agent-2d+set-navigation-layers)

(defgproperty navigation-agent-2d+pathfinding-algorithm 'navigation-agent-2d
 :get 'navigation-agent-2d+get-pathfinding-algorithm :set
 'navigation-agent-2d+set-pathfinding-algorithm)

(defgproperty navigation-agent-2d+path-postprocessing 'navigation-agent-2d :get
 'navigation-agent-2d+get-path-postprocessing :set
 'navigation-agent-2d+set-path-postprocessing)

(defgproperty navigation-agent-2d+path-metadata-flags 'navigation-agent-2d :get
 'navigation-agent-2d+get-path-metadata-flags :set
 'navigation-agent-2d+set-path-metadata-flags)

(defgproperty navigation-agent-2d+simplify-path 'navigation-agent-2d :get
 'navigation-agent-2d+get-simplify-path :set
 'navigation-agent-2d+set-simplify-path)

(defgproperty navigation-agent-2d+simplify-epsilon 'navigation-agent-2d :get
 'navigation-agent-2d+get-simplify-epsilon :set
 'navigation-agent-2d+set-simplify-epsilon)

(defgproperty navigation-agent-2d+path-return-max-length 'navigation-agent-2d
 :get 'navigation-agent-2d+get-path-return-max-length :set
 'navigation-agent-2d+set-path-return-max-length)

(defgproperty navigation-agent-2d+path-return-max-radius 'navigation-agent-2d
 :get 'navigation-agent-2d+get-path-return-max-radius :set
 'navigation-agent-2d+set-path-return-max-radius)

(defgproperty navigation-agent-2d+path-search-max-polygons 'navigation-agent-2d
 :get 'navigation-agent-2d+get-path-search-max-polygons :set
 'navigation-agent-2d+set-path-search-max-polygons)

(defgproperty navigation-agent-2d+path-search-max-distance 'navigation-agent-2d
 :get 'navigation-agent-2d+get-path-search-max-distance :set
 'navigation-agent-2d+set-path-search-max-distance)

(defgproperty navigation-agent-2d+avoidance-enabled 'navigation-agent-2d :get
 'navigation-agent-2d+get-avoidance-enabled :set
 'navigation-agent-2d+set-avoidance-enabled)

(defgproperty navigation-agent-2d+velocity 'navigation-agent-2d :get
 'navigation-agent-2d+get-velocity :set 'navigation-agent-2d+set-velocity)

(defgproperty navigation-agent-2d+radius 'navigation-agent-2d :get
 'navigation-agent-2d+get-radius :set 'navigation-agent-2d+set-radius)

(defgproperty navigation-agent-2d+neighbor-distance 'navigation-agent-2d :get
 'navigation-agent-2d+get-neighbor-distance :set
 'navigation-agent-2d+set-neighbor-distance)

(defgproperty navigation-agent-2d+max-neighbors 'navigation-agent-2d :get
 'navigation-agent-2d+get-max-neighbors :set
 'navigation-agent-2d+set-max-neighbors)

(defgproperty navigation-agent-2d+time-horizon-agents 'navigation-agent-2d :get
 'navigation-agent-2d+get-time-horizon-agents :set
 'navigation-agent-2d+set-time-horizon-agents)

(defgproperty navigation-agent-2d+time-horizon-obstacles 'navigation-agent-2d
 :get 'navigation-agent-2d+get-time-horizon-obstacles :set
 'navigation-agent-2d+set-time-horizon-obstacles)

(defgproperty navigation-agent-2d+max-speed 'navigation-agent-2d :get
 'navigation-agent-2d+get-max-speed :set 'navigation-agent-2d+set-max-speed)

(defgproperty navigation-agent-2d+avoidance-layers 'navigation-agent-2d :get
 'navigation-agent-2d+get-avoidance-layers :set
 'navigation-agent-2d+set-avoidance-layers)

(defgproperty navigation-agent-2d+avoidance-mask 'navigation-agent-2d :get
 'navigation-agent-2d+get-avoidance-mask :set
 'navigation-agent-2d+set-avoidance-mask)

(defgproperty navigation-agent-2d+avoidance-priority 'navigation-agent-2d :get
 'navigation-agent-2d+get-avoidance-priority :set
 'navigation-agent-2d+set-avoidance-priority)

(defgproperty navigation-agent-2d+debug-enabled 'navigation-agent-2d :get
 'navigation-agent-2d+get-debug-enabled :set
 'navigation-agent-2d+set-debug-enabled)

(defgproperty navigation-agent-2d+debug-use-custom 'navigation-agent-2d :get
 'navigation-agent-2d+get-debug-use-custom :set
 'navigation-agent-2d+set-debug-use-custom)

(defgproperty navigation-agent-2d+debug-path-custom-color 'navigation-agent-2d
 :get 'navigation-agent-2d+get-debug-path-custom-color :set
 'navigation-agent-2d+set-debug-path-custom-color)

(defgproperty navigation-agent-2d+debug-path-custom-point-size
 'navigation-agent-2d :get
 'navigation-agent-2d+get-debug-path-custom-point-size :set
 'navigation-agent-2d+set-debug-path-custom-point-size)

(defgproperty navigation-agent-2d+debug-path-custom-line-width
 'navigation-agent-2d :get
 'navigation-agent-2d+get-debug-path-custom-line-width :set
 'navigation-agent-2d+set-debug-path-custom-line-width)