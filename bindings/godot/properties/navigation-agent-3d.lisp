(common-lisp:in-package :%godot)


(defgproperty navigation-agent-3d+target-position 'navigation-agent-3d :get
 'navigation-agent-3d+get-target-position :set
 'navigation-agent-3d+set-target-position)

(defgproperty navigation-agent-3d+path-desired-distance 'navigation-agent-3d
 :get 'navigation-agent-3d+get-path-desired-distance :set
 'navigation-agent-3d+set-path-desired-distance)

(defgproperty navigation-agent-3d+target-desired-distance 'navigation-agent-3d
 :get 'navigation-agent-3d+get-target-desired-distance :set
 'navigation-agent-3d+set-target-desired-distance)

(defgproperty navigation-agent-3d+path-height-offset 'navigation-agent-3d :get
 'navigation-agent-3d+get-path-height-offset :set
 'navigation-agent-3d+set-path-height-offset)

(defgproperty navigation-agent-3d+path-max-distance 'navigation-agent-3d :get
 'navigation-agent-3d+get-path-max-distance :set
 'navigation-agent-3d+set-path-max-distance)

(defgproperty navigation-agent-3d+navigation-layers 'navigation-agent-3d :get
 'navigation-agent-3d+get-navigation-layers :set
 'navigation-agent-3d+set-navigation-layers)

(defgproperty navigation-agent-3d+pathfinding-algorithm 'navigation-agent-3d
 :get 'navigation-agent-3d+get-pathfinding-algorithm :set
 'navigation-agent-3d+set-pathfinding-algorithm)

(defgproperty navigation-agent-3d+path-postprocessing 'navigation-agent-3d :get
 'navigation-agent-3d+get-path-postprocessing :set
 'navigation-agent-3d+set-path-postprocessing)

(defgproperty navigation-agent-3d+path-metadata-flags 'navigation-agent-3d :get
 'navigation-agent-3d+get-path-metadata-flags :set
 'navigation-agent-3d+set-path-metadata-flags)

(defgproperty navigation-agent-3d+simplify-path 'navigation-agent-3d :get
 'navigation-agent-3d+get-simplify-path :set
 'navigation-agent-3d+set-simplify-path)

(defgproperty navigation-agent-3d+simplify-epsilon 'navigation-agent-3d :get
 'navigation-agent-3d+get-simplify-epsilon :set
 'navigation-agent-3d+set-simplify-epsilon)

(defgproperty navigation-agent-3d+path-return-max-length 'navigation-agent-3d
 :get 'navigation-agent-3d+get-path-return-max-length :set
 'navigation-agent-3d+set-path-return-max-length)

(defgproperty navigation-agent-3d+path-return-max-radius 'navigation-agent-3d
 :get 'navigation-agent-3d+get-path-return-max-radius :set
 'navigation-agent-3d+set-path-return-max-radius)

(defgproperty navigation-agent-3d+path-search-max-polygons 'navigation-agent-3d
 :get 'navigation-agent-3d+get-path-search-max-polygons :set
 'navigation-agent-3d+set-path-search-max-polygons)

(defgproperty navigation-agent-3d+path-search-max-distance 'navigation-agent-3d
 :get 'navigation-agent-3d+get-path-search-max-distance :set
 'navigation-agent-3d+set-path-search-max-distance)

(defgproperty navigation-agent-3d+avoidance-enabled 'navigation-agent-3d :get
 'navigation-agent-3d+get-avoidance-enabled :set
 'navigation-agent-3d+set-avoidance-enabled)

(defgproperty navigation-agent-3d+velocity 'navigation-agent-3d :get
 'navigation-agent-3d+get-velocity :set 'navigation-agent-3d+set-velocity)

(defgproperty navigation-agent-3d+height 'navigation-agent-3d :get
 'navigation-agent-3d+get-height :set 'navigation-agent-3d+set-height)

(defgproperty navigation-agent-3d+radius 'navigation-agent-3d :get
 'navigation-agent-3d+get-radius :set 'navigation-agent-3d+set-radius)

(defgproperty navigation-agent-3d+neighbor-distance 'navigation-agent-3d :get
 'navigation-agent-3d+get-neighbor-distance :set
 'navigation-agent-3d+set-neighbor-distance)

(defgproperty navigation-agent-3d+max-neighbors 'navigation-agent-3d :get
 'navigation-agent-3d+get-max-neighbors :set
 'navigation-agent-3d+set-max-neighbors)

(defgproperty navigation-agent-3d+time-horizon-agents 'navigation-agent-3d :get
 'navigation-agent-3d+get-time-horizon-agents :set
 'navigation-agent-3d+set-time-horizon-agents)

(defgproperty navigation-agent-3d+time-horizon-obstacles 'navigation-agent-3d
 :get 'navigation-agent-3d+get-time-horizon-obstacles :set
 'navigation-agent-3d+set-time-horizon-obstacles)

(defgproperty navigation-agent-3d+max-speed 'navigation-agent-3d :get
 'navigation-agent-3d+get-max-speed :set 'navigation-agent-3d+set-max-speed)

(defgproperty navigation-agent-3d+use-3d-avoidance 'navigation-agent-3d :get
 'navigation-agent-3d+get-use-3d-avoidance :set
 'navigation-agent-3d+set-use-3d-avoidance)

(defgproperty navigation-agent-3d+keep-y-velocity 'navigation-agent-3d :get
 'navigation-agent-3d+get-keep-y-velocity :set
 'navigation-agent-3d+set-keep-y-velocity)

(defgproperty navigation-agent-3d+avoidance-layers 'navigation-agent-3d :get
 'navigation-agent-3d+get-avoidance-layers :set
 'navigation-agent-3d+set-avoidance-layers)

(defgproperty navigation-agent-3d+avoidance-mask 'navigation-agent-3d :get
 'navigation-agent-3d+get-avoidance-mask :set
 'navigation-agent-3d+set-avoidance-mask)

(defgproperty navigation-agent-3d+avoidance-priority 'navigation-agent-3d :get
 'navigation-agent-3d+get-avoidance-priority :set
 'navigation-agent-3d+set-avoidance-priority)

(defgproperty navigation-agent-3d+debug-enabled 'navigation-agent-3d :get
 'navigation-agent-3d+get-debug-enabled :set
 'navigation-agent-3d+set-debug-enabled)

(defgproperty navigation-agent-3d+debug-use-custom 'navigation-agent-3d :get
 'navigation-agent-3d+get-debug-use-custom :set
 'navigation-agent-3d+set-debug-use-custom)

(defgproperty navigation-agent-3d+debug-path-custom-color 'navigation-agent-3d
 :get 'navigation-agent-3d+get-debug-path-custom-color :set
 'navigation-agent-3d+set-debug-path-custom-color)

(defgproperty navigation-agent-3d+debug-path-custom-point-size
 'navigation-agent-3d :get
 'navigation-agent-3d+get-debug-path-custom-point-size :set
 'navigation-agent-3d+set-debug-path-custom-point-size)