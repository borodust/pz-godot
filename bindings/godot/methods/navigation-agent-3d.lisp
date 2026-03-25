(common-lisp:in-package :%godot)


(defgmethod
 (navigation-agent-3d+get-rid :class 'navigation-agent-3d :bind "get_rid" :hash
  2944877500)
 rid)

(defgmethod
 (navigation-agent-3d+set-avoidance-enabled :class 'navigation-agent-3d :bind
  "set_avoidance_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-agent-3d+get-avoidance-enabled :class 'navigation-agent-3d :bind
  "get_avoidance_enabled" :hash 36873697)
 bool)

(defgmethod
 (navigation-agent-3d+set-path-desired-distance :class 'navigation-agent-3d
  :bind "set_path_desired_distance" :hash 373806689)
 :void (desired-distance float))

(defgmethod
 (navigation-agent-3d+get-path-desired-distance :class 'navigation-agent-3d
  :bind "get_path_desired_distance" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-target-desired-distance :class 'navigation-agent-3d
  :bind "set_target_desired_distance" :hash 373806689)
 :void (desired-distance float))

(defgmethod
 (navigation-agent-3d+get-target-desired-distance :class 'navigation-agent-3d
  :bind "get_target_desired_distance" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-radius :class 'navigation-agent-3d :bind "set_radius"
  :hash 373806689)
 :void (radius float))

(defgmethod
 (navigation-agent-3d+get-radius :class 'navigation-agent-3d :bind "get_radius"
  :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-height :class 'navigation-agent-3d :bind "set_height"
  :hash 373806689)
 :void (height float))

(defgmethod
 (navigation-agent-3d+get-height :class 'navigation-agent-3d :bind "get_height"
  :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-path-height-offset :class 'navigation-agent-3d :bind
  "set_path_height_offset" :hash 373806689)
 :void (path-height-offset float))

(defgmethod
 (navigation-agent-3d+get-path-height-offset :class 'navigation-agent-3d :bind
  "get_path_height_offset" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-use-3d-avoidance :class 'navigation-agent-3d :bind
  "set_use_3d_avoidance" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-agent-3d+get-use-3d-avoidance :class 'navigation-agent-3d :bind
  "get_use_3d_avoidance" :hash 36873697)
 bool)

(defgmethod
 (navigation-agent-3d+set-keep-y-velocity :class 'navigation-agent-3d :bind
  "set_keep_y_velocity" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-agent-3d+get-keep-y-velocity :class 'navigation-agent-3d :bind
  "get_keep_y_velocity" :hash 36873697)
 bool)

(defgmethod
 (navigation-agent-3d+set-neighbor-distance :class 'navigation-agent-3d :bind
  "set_neighbor_distance" :hash 373806689)
 :void (neighbor-distance float))

(defgmethod
 (navigation-agent-3d+get-neighbor-distance :class 'navigation-agent-3d :bind
  "get_neighbor_distance" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-max-neighbors :class 'navigation-agent-3d :bind
  "set_max_neighbors" :hash 1286410249)
 :void (max-neighbors int))

(defgmethod
 (navigation-agent-3d+get-max-neighbors :class 'navigation-agent-3d :bind
  "get_max_neighbors" :hash 3905245786)
 int)

(defgmethod
 (navigation-agent-3d+set-time-horizon-agents :class 'navigation-agent-3d :bind
  "set_time_horizon_agents" :hash 373806689)
 :void (time-horizon float))

(defgmethod
 (navigation-agent-3d+get-time-horizon-agents :class 'navigation-agent-3d :bind
  "get_time_horizon_agents" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-time-horizon-obstacles :class 'navigation-agent-3d
  :bind "set_time_horizon_obstacles" :hash 373806689)
 :void (time-horizon float))

(defgmethod
 (navigation-agent-3d+get-time-horizon-obstacles :class 'navigation-agent-3d
  :bind "get_time_horizon_obstacles" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-max-speed :class 'navigation-agent-3d :bind
  "set_max_speed" :hash 373806689)
 :void (max-speed float))

(defgmethod
 (navigation-agent-3d+get-max-speed :class 'navigation-agent-3d :bind
  "get_max_speed" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-path-max-distance :class 'navigation-agent-3d :bind
  "set_path_max_distance" :hash 373806689)
 :void (max-speed float))

(defgmethod
 (navigation-agent-3d+get-path-max-distance :class 'navigation-agent-3d :bind
  "get_path_max_distance" :hash 191475506)
 float)

(defgmethod
 (navigation-agent-3d+set-navigation-layers :class 'navigation-agent-3d :bind
  "set_navigation_layers" :hash 1286410249)
 :void (navigation-layers int))

(defgmethod
 (navigation-agent-3d+get-navigation-layers :class 'navigation-agent-3d :bind
  "get_navigation_layers" :hash 3905245786)
 int)

(defgmethod
 (navigation-agent-3d+set-navigation-layer-value :class 'navigation-agent-3d
  :bind "set_navigation_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (navigation-agent-3d+get-navigation-layer-value :class 'navigation-agent-3d
  :bind "get_navigation_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (navigation-agent-3d+set-pathfinding-algorithm :class 'navigation-agent-3d
  :bind "set_pathfinding_algorithm" :hash 394560454)
 :void
 (pathfinding-algorithm
  navigation-path-query-parameters-3d+pathfinding-algorithm))

(defgmethod
 (navigation-agent-3d+get-pathfinding-algorithm :class 'navigation-agent-3d
  :bind "get_pathfinding_algorithm" :hash 3398491350)
 navigation-path-query-parameters-3d+pathfinding-algorithm)

(defgmethod
 (navigation-agent-3d+set-path-postprocessing :class 'navigation-agent-3d :bind
  "set_path_postprocessing" :hash 2267362344)
 :void
 (path-postprocessing navigation-path-query-parameters-3d+path-post-processing))

(defgmethod
 (navigation-agent-3d+get-path-postprocessing :class 'navigation-agent-3d :bind
  "get_path_postprocessing" :hash 3883858360)
 navigation-path-query-parameters-3d+path-post-processing)

(defgmethod
 (navigation-agent-3d+set-path-metadata-flags :class 'navigation-agent-3d :bind
  "set_path_metadata_flags" :hash 2713846708)
 :void (flags navigation-path-query-parameters-3d+path-metadata-flags))

(defgmethod
 (navigation-agent-3d+get-path-metadata-flags :class 'navigation-agent-3d :bind
  "get_path_metadata_flags" :hash 1582332802)
 navigation-path-query-parameters-3d+path-metadata-flags)

(defgmethod
 (navigation-agent-3d+set-navigation-map :class 'navigation-agent-3d :bind
  "set_navigation_map" :hash 2722037293)
 :void (navigation-map rid))

(defgmethod
 (navigation-agent-3d+get-navigation-map :class 'navigation-agent-3d :bind
  "get_navigation_map" :hash 2944877500)
 rid)

(defgmethod
 (navigation-agent-3d+set-target-position :class 'navigation-agent-3d :bind
  "set_target_position" :hash 3460891852)
 :void (position vector-3))

(defgmethod
 (navigation-agent-3d+get-target-position :class 'navigation-agent-3d :bind
  "get_target_position" :hash 3360562783)
 vector-3)

(defgmethod
 (navigation-agent-3d+set-simplify-path :class 'navigation-agent-3d :bind
  "set_simplify_path" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-agent-3d+get-simplify-path :class 'navigation-agent-3d :bind
  "get_simplify_path" :hash 36873697)
 bool)

(defgmethod
 (navigation-agent-3d+set-simplify-epsilon :class 'navigation-agent-3d :bind
  "set_simplify_epsilon" :hash 373806689)
 :void (epsilon float))

(defgmethod
 (navigation-agent-3d+get-simplify-epsilon :class 'navigation-agent-3d :bind
  "get_simplify_epsilon" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-path-return-max-length :class 'navigation-agent-3d
  :bind "set_path_return_max_length" :hash 373806689)
 :void (length float))

(defgmethod
 (navigation-agent-3d+get-path-return-max-length :class 'navigation-agent-3d
  :bind "get_path_return_max_length" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-path-return-max-radius :class 'navigation-agent-3d
  :bind "set_path_return_max_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (navigation-agent-3d+get-path-return-max-radius :class 'navigation-agent-3d
  :bind "get_path_return_max_radius" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-path-search-max-polygons :class 'navigation-agent-3d
  :bind "set_path_search_max_polygons" :hash 1286410249)
 :void (max-polygons int))

(defgmethod
 (navigation-agent-3d+get-path-search-max-polygons :class 'navigation-agent-3d
  :bind "get_path_search_max_polygons" :hash 3905245786)
 int)

(defgmethod
 (navigation-agent-3d+set-path-search-max-distance :class 'navigation-agent-3d
  :bind "set_path_search_max_distance" :hash 373806689)
 :void (distance float))

(defgmethod
 (navigation-agent-3d+get-path-search-max-distance :class 'navigation-agent-3d
  :bind "get_path_search_max_distance" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+get-path-length :class 'navigation-agent-3d :bind
  "get_path_length" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+get-next-path-position :class 'navigation-agent-3d :bind
  "get_next_path_position" :hash 3783033775)
 vector-3)

(defgmethod
 (navigation-agent-3d+set-velocity-forced :class 'navigation-agent-3d :bind
  "set_velocity_forced" :hash 3460891852)
 :void (velocity vector-3))

(defgmethod
 (navigation-agent-3d+set-velocity :class 'navigation-agent-3d :bind
  "set_velocity" :hash 3460891852)
 :void (velocity vector-3))

(defgmethod
 (navigation-agent-3d+get-velocity :class 'navigation-agent-3d :bind
  "get_velocity" :hash 3783033775)
 vector-3)

(defgmethod
 (navigation-agent-3d+distance-to-target :class 'navigation-agent-3d :bind
  "distance_to_target" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+get-current-navigation-result :class 'navigation-agent-3d
  :bind "get_current_navigation_result" :hash 728825684)
 navigation-path-query-result-3d)

(defgmethod
 (navigation-agent-3d+get-current-navigation-path :class 'navigation-agent-3d
  :bind "get_current_navigation_path" :hash 497664490)
 packed-vector-3array)

(defgmethod
 (navigation-agent-3d+get-current-navigation-path-index :class
  'navigation-agent-3d :bind "get_current_navigation_path_index" :hash
  3905245786)
 int)

(defgmethod
 (navigation-agent-3d+is-target-reached :class 'navigation-agent-3d :bind
  "is_target_reached" :hash 36873697)
 bool)

(defgmethod
 (navigation-agent-3d+is-target-reachable :class 'navigation-agent-3d :bind
  "is_target_reachable" :hash 2240911060)
 bool)

(defgmethod
 (navigation-agent-3d+is-navigation-finished :class 'navigation-agent-3d :bind
  "is_navigation_finished" :hash 2240911060)
 bool)

(defgmethod
 (navigation-agent-3d+get-final-position :class 'navigation-agent-3d :bind
  "get_final_position" :hash 3783033775)
 vector-3)

(defgmethod
 (navigation-agent-3d+set-avoidance-layers :class 'navigation-agent-3d :bind
  "set_avoidance_layers" :hash 1286410249)
 :void (layers int))

(defgmethod
 (navigation-agent-3d+get-avoidance-layers :class 'navigation-agent-3d :bind
  "get_avoidance_layers" :hash 3905245786)
 int)

(defgmethod
 (navigation-agent-3d+set-avoidance-mask :class 'navigation-agent-3d :bind
  "set_avoidance_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (navigation-agent-3d+get-avoidance-mask :class 'navigation-agent-3d :bind
  "get_avoidance_mask" :hash 3905245786)
 int)

(defgmethod
 (navigation-agent-3d+set-avoidance-layer-value :class 'navigation-agent-3d
  :bind "set_avoidance_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (navigation-agent-3d+get-avoidance-layer-value :class 'navigation-agent-3d
  :bind "get_avoidance_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (navigation-agent-3d+set-avoidance-mask-value :class 'navigation-agent-3d
  :bind "set_avoidance_mask_value" :hash 300928843)
 :void (mask-number int) (value bool))

(defgmethod
 (navigation-agent-3d+get-avoidance-mask-value :class 'navigation-agent-3d
  :bind "get_avoidance_mask_value" :hash 1116898809)
 bool (mask-number int))

(defgmethod
 (navigation-agent-3d+set-avoidance-priority :class 'navigation-agent-3d :bind
  "set_avoidance_priority" :hash 373806689)
 :void (priority float))

(defgmethod
 (navigation-agent-3d+get-avoidance-priority :class 'navigation-agent-3d :bind
  "get_avoidance_priority" :hash 1740695150)
 float)

(defgmethod
 (navigation-agent-3d+set-debug-enabled :class 'navigation-agent-3d :bind
  "set_debug_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-agent-3d+get-debug-enabled :class 'navigation-agent-3d :bind
  "get_debug_enabled" :hash 36873697)
 bool)

(defgmethod
 (navigation-agent-3d+set-debug-use-custom :class 'navigation-agent-3d :bind
  "set_debug_use_custom" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-agent-3d+get-debug-use-custom :class 'navigation-agent-3d :bind
  "get_debug_use_custom" :hash 36873697)
 bool)

(defgmethod
 (navigation-agent-3d+set-debug-path-custom-color :class 'navigation-agent-3d
  :bind "set_debug_path_custom_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (navigation-agent-3d+get-debug-path-custom-color :class 'navigation-agent-3d
  :bind "get_debug_path_custom_color" :hash 3444240500)
 color)

(defgmethod
 (navigation-agent-3d+set-debug-path-custom-point-size :class
  'navigation-agent-3d :bind "set_debug_path_custom_point_size" :hash
  373806689)
 :void (point-size float))

(defgmethod
 (navigation-agent-3d+get-debug-path-custom-point-size :class
  'navigation-agent-3d :bind "get_debug_path_custom_point_size" :hash
  1740695150)
 float)