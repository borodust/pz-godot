(common-lisp:in-package :%godot)


(defgmethod
 (navigation-server-2d+get-maps :class 'navigation-server-2d :bind "get_maps"
  :hash 3995934104)
 array)

(defgmethod
 (navigation-server-2d+map-create :class 'navigation-server-2d :bind
  "map_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-2d+map-set-active :class 'navigation-server-2d :bind
  "map_set_active" :hash 1265174801)
 :void (map rid) (active bool))

(defgmethod
 (navigation-server-2d+map-is-active :class 'navigation-server-2d :bind
  "map_is_active" :hash 4155700596)
 bool (map rid))

(defgmethod
 (navigation-server-2d+map-set-cell-size :class 'navigation-server-2d :bind
  "map_set_cell_size" :hash 1794382983)
 :void (map rid) (cell-size float))

(defgmethod
 (navigation-server-2d+map-get-cell-size :class 'navigation-server-2d :bind
  "map_get_cell_size" :hash 866169185)
 float (map rid))

(defgmethod
 (navigation-server-2d+map-set-merge-rasterizer-cell-scale :class
  'navigation-server-2d :bind "map_set_merge_rasterizer_cell_scale" :hash
  1794382983)
 :void (map rid) (scale float))

(defgmethod
 (navigation-server-2d+map-get-merge-rasterizer-cell-scale :class
  'navigation-server-2d :bind "map_get_merge_rasterizer_cell_scale" :hash
  866169185)
 float (map rid))

(defgmethod
 (navigation-server-2d+map-set-use-edge-connections :class
  'navigation-server-2d :bind "map_set_use_edge_connections" :hash 1265174801)
 :void (map rid) (enabled bool))

(defgmethod
 (navigation-server-2d+map-get-use-edge-connections :class
  'navigation-server-2d :bind "map_get_use_edge_connections" :hash 4155700596)
 bool (map rid))

(defgmethod
 (navigation-server-2d+map-set-edge-connection-margin :class
  'navigation-server-2d :bind "map_set_edge_connection_margin" :hash
  1794382983)
 :void (map rid) (margin float))

(defgmethod
 (navigation-server-2d+map-get-edge-connection-margin :class
  'navigation-server-2d :bind "map_get_edge_connection_margin" :hash 866169185)
 float (map rid))

(defgmethod
 (navigation-server-2d+map-set-link-connection-radius :class
  'navigation-server-2d :bind "map_set_link_connection_radius" :hash
  1794382983)
 :void (map rid) (radius float))

(defgmethod
 (navigation-server-2d+map-get-link-connection-radius :class
  'navigation-server-2d :bind "map_get_link_connection_radius" :hash 866169185)
 float (map rid))

(defgmethod
 (navigation-server-2d+map-get-path :class 'navigation-server-2d :bind
  "map_get_path" :hash 1279824844)
 packed-vector-2array (map rid) (origin vector-2) (destination vector-2)
 (optimize bool) (navigation-layers int))

(defgmethod
 (navigation-server-2d+map-get-closest-point :class 'navigation-server-2d :bind
  "map_get_closest_point" :hash 1358334418)
 vector-2 (map rid) (to-point vector-2))

(defgmethod
 (navigation-server-2d+map-get-closest-point-owner :class 'navigation-server-2d
  :bind "map_get_closest_point_owner" :hash 1353467510)
 rid (map rid) (to-point vector-2))

(defgmethod
 (navigation-server-2d+map-get-links :class 'navigation-server-2d :bind
  "map_get_links" :hash 2684255073)
 array (map rid))

(defgmethod
 (navigation-server-2d+map-get-regions :class 'navigation-server-2d :bind
  "map_get_regions" :hash 2684255073)
 array (map rid))

(defgmethod
 (navigation-server-2d+map-get-agents :class 'navigation-server-2d :bind
  "map_get_agents" :hash 2684255073)
 array (map rid))

(defgmethod
 (navigation-server-2d+map-get-obstacles :class 'navigation-server-2d :bind
  "map_get_obstacles" :hash 2684255073)
 array (map rid))

(defgmethod
 (navigation-server-2d+map-force-update :class 'navigation-server-2d :bind
  "map_force_update" :hash 2722037293)
 :void (map rid))

(defgmethod
 (navigation-server-2d+map-get-iteration-id :class 'navigation-server-2d :bind
  "map_get_iteration_id" :hash 2198884583)
 int (map rid))

(defgmethod
 (navigation-server-2d+map-set-use-async-iterations :class
  'navigation-server-2d :bind "map_set_use_async_iterations" :hash 1265174801)
 :void (map rid) (enabled bool))

(defgmethod
 (navigation-server-2d+map-get-use-async-iterations :class
  'navigation-server-2d :bind "map_get_use_async_iterations" :hash 4155700596)
 bool (map rid))

(defgmethod
 (navigation-server-2d+map-get-random-point :class 'navigation-server-2d :bind
  "map_get_random_point" :hash 3271000763)
 vector-2 (map rid) (navigation-layers int) (uniformly bool))

(defgmethod
 (navigation-server-2d+query-path :class 'navigation-server-2d :bind
  "query_path" :hash 1254915886)
 :void (parameters navigation-path-query-parameters-2d)
 (result navigation-path-query-result-2d) (callback callable))

(defgmethod
 (navigation-server-2d+region-create :class 'navigation-server-2d :bind
  "region_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-2d+region-get-iteration-id :class 'navigation-server-2d
  :bind "region_get_iteration_id" :hash 2198884583)
 int (region rid))

(defgmethod
 (navigation-server-2d+region-set-use-async-iterations :class
  'navigation-server-2d :bind "region_set_use_async_iterations" :hash
  1265174801)
 :void (region rid) (enabled bool))

(defgmethod
 (navigation-server-2d+region-get-use-async-iterations :class
  'navigation-server-2d :bind "region_get_use_async_iterations" :hash
  4155700596)
 bool (region rid))

(defgmethod
 (navigation-server-2d+region-set-enabled :class 'navigation-server-2d :bind
  "region_set_enabled" :hash 1265174801)
 :void (region rid) (enabled bool))

(defgmethod
 (navigation-server-2d+region-get-enabled :class 'navigation-server-2d :bind
  "region_get_enabled" :hash 4155700596)
 bool (region rid))

(defgmethod
 (navigation-server-2d+region-set-use-edge-connections :class
  'navigation-server-2d :bind "region_set_use_edge_connections" :hash
  1265174801)
 :void (region rid) (enabled bool))

(defgmethod
 (navigation-server-2d+region-get-use-edge-connections :class
  'navigation-server-2d :bind "region_get_use_edge_connections" :hash
  4155700596)
 bool (region rid))

(defgmethod
 (navigation-server-2d+region-set-enter-cost :class 'navigation-server-2d :bind
  "region_set_enter_cost" :hash 1794382983)
 :void (region rid) (enter-cost float))

(defgmethod
 (navigation-server-2d+region-get-enter-cost :class 'navigation-server-2d :bind
  "region_get_enter_cost" :hash 866169185)
 float (region rid))

(defgmethod
 (navigation-server-2d+region-set-travel-cost :class 'navigation-server-2d
  :bind "region_set_travel_cost" :hash 1794382983)
 :void (region rid) (travel-cost float))

(defgmethod
 (navigation-server-2d+region-get-travel-cost :class 'navigation-server-2d
  :bind "region_get_travel_cost" :hash 866169185)
 float (region rid))

(defgmethod
 (navigation-server-2d+region-set-owner-id :class 'navigation-server-2d :bind
  "region_set_owner_id" :hash 3411492887)
 :void (region rid) (owner-id int))

(defgmethod
 (navigation-server-2d+region-get-owner-id :class 'navigation-server-2d :bind
  "region_get_owner_id" :hash 2198884583)
 int (region rid))

(defgmethod
 (navigation-server-2d+region-owns-point :class 'navigation-server-2d :bind
  "region_owns_point" :hash 219849798)
 bool (region rid) (point vector-2))

(defgmethod
 (navigation-server-2d+region-set-map :class 'navigation-server-2d :bind
  "region_set_map" :hash 395945892)
 :void (region rid) (map rid))

(defgmethod
 (navigation-server-2d+region-get-map :class 'navigation-server-2d :bind
  "region_get_map" :hash 3814569979)
 rid (region rid))

(defgmethod
 (navigation-server-2d+region-set-navigation-layers :class
  'navigation-server-2d :bind "region_set_navigation_layers" :hash 3411492887)
 :void (region rid) (navigation-layers int))

(defgmethod
 (navigation-server-2d+region-get-navigation-layers :class
  'navigation-server-2d :bind "region_get_navigation_layers" :hash 2198884583)
 int (region rid))

(defgmethod
 (navigation-server-2d+region-set-transform :class 'navigation-server-2d :bind
  "region_set_transform" :hash 1246044741)
 :void (region rid) (transform transform-2d))

(defgmethod
 (navigation-server-2d+region-get-transform :class 'navigation-server-2d :bind
  "region_get_transform" :hash 213527486)
 transform-2d (region rid))

(defgmethod
 (navigation-server-2d+region-set-navigation-polygon :class
  'navigation-server-2d :bind "region_set_navigation_polygon" :hash 3633623451)
 :void (region rid) (navigation-polygon navigation-polygon))

(defgmethod
 (navigation-server-2d+region-get-connections-count :class
  'navigation-server-2d :bind "region_get_connections_count" :hash 2198884583)
 int (region rid))

(defgmethod
 (navigation-server-2d+region-get-connection-pathway-start :class
  'navigation-server-2d :bind "region_get_connection_pathway_start" :hash
  2546185844)
 vector-2 (region rid) (connection int))

(defgmethod
 (navigation-server-2d+region-get-connection-pathway-end :class
  'navigation-server-2d :bind "region_get_connection_pathway_end" :hash
  2546185844)
 vector-2 (region rid) (connection int))

(defgmethod
 (navigation-server-2d+region-get-closest-point :class 'navigation-server-2d
  :bind "region_get_closest_point" :hash 1358334418)
 vector-2 (region rid) (to-point vector-2))

(defgmethod
 (navigation-server-2d+region-get-random-point :class 'navigation-server-2d
  :bind "region_get_random_point" :hash 3271000763)
 vector-2 (region rid) (navigation-layers int) (uniformly bool))

(defgmethod
 (navigation-server-2d+region-get-bounds :class 'navigation-server-2d :bind
  "region_get_bounds" :hash 1097232729)
 rect-2 (region rid))

(defgmethod
 (navigation-server-2d+link-create :class 'navigation-server-2d :bind
  "link_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-2d+link-get-iteration-id :class 'navigation-server-2d :bind
  "link_get_iteration_id" :hash 2198884583)
 int (link rid))

(defgmethod
 (navigation-server-2d+link-set-map :class 'navigation-server-2d :bind
  "link_set_map" :hash 395945892)
 :void (link rid) (map rid))

(defgmethod
 (navigation-server-2d+link-get-map :class 'navigation-server-2d :bind
  "link_get_map" :hash 3814569979)
 rid (link rid))

(defgmethod
 (navigation-server-2d+link-set-enabled :class 'navigation-server-2d :bind
  "link_set_enabled" :hash 1265174801)
 :void (link rid) (enabled bool))

(defgmethod
 (navigation-server-2d+link-get-enabled :class 'navigation-server-2d :bind
  "link_get_enabled" :hash 4155700596)
 bool (link rid))

(defgmethod
 (navigation-server-2d+link-set-bidirectional :class 'navigation-server-2d
  :bind "link_set_bidirectional" :hash 1265174801)
 :void (link rid) (bidirectional bool))

(defgmethod
 (navigation-server-2d+link-is-bidirectional :class 'navigation-server-2d :bind
  "link_is_bidirectional" :hash 4155700596)
 bool (link rid))

(defgmethod
 (navigation-server-2d+link-set-navigation-layers :class 'navigation-server-2d
  :bind "link_set_navigation_layers" :hash 3411492887)
 :void (link rid) (navigation-layers int))

(defgmethod
 (navigation-server-2d+link-get-navigation-layers :class 'navigation-server-2d
  :bind "link_get_navigation_layers" :hash 2198884583)
 int (link rid))

(defgmethod
 (navigation-server-2d+link-set-start-position :class 'navigation-server-2d
  :bind "link_set_start_position" :hash 3201125042)
 :void (link rid) (position vector-2))

(defgmethod
 (navigation-server-2d+link-get-start-position :class 'navigation-server-2d
  :bind "link_get_start_position" :hash 2440833711)
 vector-2 (link rid))

(defgmethod
 (navigation-server-2d+link-set-end-position :class 'navigation-server-2d :bind
  "link_set_end_position" :hash 3201125042)
 :void (link rid) (position vector-2))

(defgmethod
 (navigation-server-2d+link-get-end-position :class 'navigation-server-2d :bind
  "link_get_end_position" :hash 2440833711)
 vector-2 (link rid))

(defgmethod
 (navigation-server-2d+link-set-enter-cost :class 'navigation-server-2d :bind
  "link_set_enter_cost" :hash 1794382983)
 :void (link rid) (enter-cost float))

(defgmethod
 (navigation-server-2d+link-get-enter-cost :class 'navigation-server-2d :bind
  "link_get_enter_cost" :hash 866169185)
 float (link rid))

(defgmethod
 (navigation-server-2d+link-set-travel-cost :class 'navigation-server-2d :bind
  "link_set_travel_cost" :hash 1794382983)
 :void (link rid) (travel-cost float))

(defgmethod
 (navigation-server-2d+link-get-travel-cost :class 'navigation-server-2d :bind
  "link_get_travel_cost" :hash 866169185)
 float (link rid))

(defgmethod
 (navigation-server-2d+link-set-owner-id :class 'navigation-server-2d :bind
  "link_set_owner_id" :hash 3411492887)
 :void (link rid) (owner-id int))

(defgmethod
 (navigation-server-2d+link-get-owner-id :class 'navigation-server-2d :bind
  "link_get_owner_id" :hash 2198884583)
 int (link rid))

(defgmethod
 (navigation-server-2d+agent-create :class 'navigation-server-2d :bind
  "agent_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-2d+agent-set-avoidance-enabled :class 'navigation-server-2d
  :bind "agent_set_avoidance_enabled" :hash 1265174801)
 :void (agent rid) (enabled bool))

(defgmethod
 (navigation-server-2d+agent-get-avoidance-enabled :class 'navigation-server-2d
  :bind "agent_get_avoidance_enabled" :hash 4155700596)
 bool (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-map :class 'navigation-server-2d :bind
  "agent_set_map" :hash 395945892)
 :void (agent rid) (map rid))

(defgmethod
 (navigation-server-2d+agent-get-map :class 'navigation-server-2d :bind
  "agent_get_map" :hash 3814569979)
 rid (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-paused :class 'navigation-server-2d :bind
  "agent_set_paused" :hash 1265174801)
 :void (agent rid) (paused bool))

(defgmethod
 (navigation-server-2d+agent-get-paused :class 'navigation-server-2d :bind
  "agent_get_paused" :hash 4155700596)
 bool (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-neighbor-distance :class 'navigation-server-2d
  :bind "agent_set_neighbor_distance" :hash 1794382983)
 :void (agent rid) (distance float))

(defgmethod
 (navigation-server-2d+agent-get-neighbor-distance :class 'navigation-server-2d
  :bind "agent_get_neighbor_distance" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-max-neighbors :class 'navigation-server-2d
  :bind "agent_set_max_neighbors" :hash 3411492887)
 :void (agent rid) (count int))

(defgmethod
 (navigation-server-2d+agent-get-max-neighbors :class 'navigation-server-2d
  :bind "agent_get_max_neighbors" :hash 2198884583)
 int (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-time-horizon-agents :class
  'navigation-server-2d :bind "agent_set_time_horizon_agents" :hash 1794382983)
 :void (agent rid) (time-horizon float))

(defgmethod
 (navigation-server-2d+agent-get-time-horizon-agents :class
  'navigation-server-2d :bind "agent_get_time_horizon_agents" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-time-horizon-obstacles :class
  'navigation-server-2d :bind "agent_set_time_horizon_obstacles" :hash
  1794382983)
 :void (agent rid) (time-horizon float))

(defgmethod
 (navigation-server-2d+agent-get-time-horizon-obstacles :class
  'navigation-server-2d :bind "agent_get_time_horizon_obstacles" :hash
  866169185)
 float (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-radius :class 'navigation-server-2d :bind
  "agent_set_radius" :hash 1794382983)
 :void (agent rid) (radius float))

(defgmethod
 (navigation-server-2d+agent-get-radius :class 'navigation-server-2d :bind
  "agent_get_radius" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-max-speed :class 'navigation-server-2d :bind
  "agent_set_max_speed" :hash 1794382983)
 :void (agent rid) (max-speed float))

(defgmethod
 (navigation-server-2d+agent-get-max-speed :class 'navigation-server-2d :bind
  "agent_get_max_speed" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-velocity-forced :class 'navigation-server-2d
  :bind "agent_set_velocity_forced" :hash 3201125042)
 :void (agent rid) (velocity vector-2))

(defgmethod
 (navigation-server-2d+agent-set-velocity :class 'navigation-server-2d :bind
  "agent_set_velocity" :hash 3201125042)
 :void (agent rid) (velocity vector-2))

(defgmethod
 (navigation-server-2d+agent-get-velocity :class 'navigation-server-2d :bind
  "agent_get_velocity" :hash 2440833711)
 vector-2 (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-position :class 'navigation-server-2d :bind
  "agent_set_position" :hash 3201125042)
 :void (agent rid) (position vector-2))

(defgmethod
 (navigation-server-2d+agent-get-position :class 'navigation-server-2d :bind
  "agent_get_position" :hash 2440833711)
 vector-2 (agent rid))

(defgmethod
 (navigation-server-2d+agent-is-map-changed :class 'navigation-server-2d :bind
  "agent_is_map_changed" :hash 4155700596)
 bool (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-avoidance-callback :class
  'navigation-server-2d :bind "agent_set_avoidance_callback" :hash 3379118538)
 :void (agent rid) (callback callable))

(defgmethod
 (navigation-server-2d+agent-has-avoidance-callback :class
  'navigation-server-2d :bind "agent_has_avoidance_callback" :hash 4155700596)
 bool (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-avoidance-layers :class 'navigation-server-2d
  :bind "agent_set_avoidance_layers" :hash 3411492887)
 :void (agent rid) (layers int))

(defgmethod
 (navigation-server-2d+agent-get-avoidance-layers :class 'navigation-server-2d
  :bind "agent_get_avoidance_layers" :hash 2198884583)
 int (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-avoidance-mask :class 'navigation-server-2d
  :bind "agent_set_avoidance_mask" :hash 3411492887)
 :void (agent rid) (mask int))

(defgmethod
 (navigation-server-2d+agent-get-avoidance-mask :class 'navigation-server-2d
  :bind "agent_get_avoidance_mask" :hash 2198884583)
 int (agent rid))

(defgmethod
 (navigation-server-2d+agent-set-avoidance-priority :class
  'navigation-server-2d :bind "agent_set_avoidance_priority" :hash 1794382983)
 :void (agent rid) (priority float))

(defgmethod
 (navigation-server-2d+agent-get-avoidance-priority :class
  'navigation-server-2d :bind "agent_get_avoidance_priority" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-2d+obstacle-create :class 'navigation-server-2d :bind
  "obstacle_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-2d+obstacle-set-avoidance-enabled :class
  'navigation-server-2d :bind "obstacle_set_avoidance_enabled" :hash
  1265174801)
 :void (obstacle rid) (enabled bool))

(defgmethod
 (navigation-server-2d+obstacle-get-avoidance-enabled :class
  'navigation-server-2d :bind "obstacle_get_avoidance_enabled" :hash
  4155700596)
 bool (obstacle rid))

(defgmethod
 (navigation-server-2d+obstacle-set-map :class 'navigation-server-2d :bind
  "obstacle_set_map" :hash 395945892)
 :void (obstacle rid) (map rid))

(defgmethod
 (navigation-server-2d+obstacle-get-map :class 'navigation-server-2d :bind
  "obstacle_get_map" :hash 3814569979)
 rid (obstacle rid))

(defgmethod
 (navigation-server-2d+obstacle-set-paused :class 'navigation-server-2d :bind
  "obstacle_set_paused" :hash 1265174801)
 :void (obstacle rid) (paused bool))

(defgmethod
 (navigation-server-2d+obstacle-get-paused :class 'navigation-server-2d :bind
  "obstacle_get_paused" :hash 4155700596)
 bool (obstacle rid))

(defgmethod
 (navigation-server-2d+obstacle-set-radius :class 'navigation-server-2d :bind
  "obstacle_set_radius" :hash 1794382983)
 :void (obstacle rid) (radius float))

(defgmethod
 (navigation-server-2d+obstacle-get-radius :class 'navigation-server-2d :bind
  "obstacle_get_radius" :hash 866169185)
 float (obstacle rid))

(defgmethod
 (navigation-server-2d+obstacle-set-velocity :class 'navigation-server-2d :bind
  "obstacle_set_velocity" :hash 3201125042)
 :void (obstacle rid) (velocity vector-2))

(defgmethod
 (navigation-server-2d+obstacle-get-velocity :class 'navigation-server-2d :bind
  "obstacle_get_velocity" :hash 2440833711)
 vector-2 (obstacle rid))

(defgmethod
 (navigation-server-2d+obstacle-set-position :class 'navigation-server-2d :bind
  "obstacle_set_position" :hash 3201125042)
 :void (obstacle rid) (position vector-2))

(defgmethod
 (navigation-server-2d+obstacle-get-position :class 'navigation-server-2d :bind
  "obstacle_get_position" :hash 2440833711)
 vector-2 (obstacle rid))

(defgmethod
 (navigation-server-2d+obstacle-set-vertices :class 'navigation-server-2d :bind
  "obstacle_set_vertices" :hash 29476483)
 :void (obstacle rid) (vertices packed-vector-2array))

(defgmethod
 (navigation-server-2d+obstacle-get-vertices :class 'navigation-server-2d :bind
  "obstacle_get_vertices" :hash 2222557395)
 packed-vector-2array (obstacle rid))

(defgmethod
 (navigation-server-2d+obstacle-set-avoidance-layers :class
  'navigation-server-2d :bind "obstacle_set_avoidance_layers" :hash 3411492887)
 :void (obstacle rid) (layers int))

(defgmethod
 (navigation-server-2d+obstacle-get-avoidance-layers :class
  'navigation-server-2d :bind "obstacle_get_avoidance_layers" :hash 2198884583)
 int (obstacle rid))

(defgmethod
 (navigation-server-2d+parse-source-geometry-data :class 'navigation-server-2d
  :bind "parse_source_geometry_data" :hash 1766905497)
 :void (navigation-polygon navigation-polygon)
 (source-geometry-data navigation-mesh-source-geometry-data-2d)
 (root-node node) (callback callable))

(defgmethod
 (navigation-server-2d+bake-from-source-geometry-data :class
  'navigation-server-2d :bind "bake_from_source_geometry_data" :hash
  2179660022)
 :void (navigation-polygon navigation-polygon)
 (source-geometry-data navigation-mesh-source-geometry-data-2d)
 (callback callable))

(defgmethod
 (navigation-server-2d+bake-from-source-geometry-data-async :class
  'navigation-server-2d :bind "bake_from_source_geometry_data_async" :hash
  2179660022)
 :void (navigation-polygon navigation-polygon)
 (source-geometry-data navigation-mesh-source-geometry-data-2d)
 (callback callable))

(defgmethod
 (navigation-server-2d+is-baking-navigation-polygon :class
  'navigation-server-2d :bind "is_baking_navigation_polygon" :hash 3729405808)
 bool (navigation-polygon navigation-polygon))

(defgmethod
 (navigation-server-2d+source-geometry-parser-create :class
  'navigation-server-2d :bind "source_geometry_parser_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-2d+source-geometry-parser-set-callback :class
  'navigation-server-2d :bind "source_geometry_parser_set_callback" :hash
  3379118538)
 :void (parser rid) (callback callable))

(defgmethod
 (navigation-server-2d+simplify-path :class 'navigation-server-2d :bind
  "simplify_path" :hash 2457191505)
 packed-vector-2array (path packed-vector-2array) (epsilon float))

(defgmethod
 (navigation-server-2d+free-rid :class 'navigation-server-2d :bind "free_rid"
  :hash 2722037293)
 :void (rid rid))

(defgmethod
 (navigation-server-2d+set-active :class 'navigation-server-2d :bind
  "set_active" :hash 2586408642)
 :void (active bool))

(defgmethod
 (navigation-server-2d+set-debug-enabled :class 'navigation-server-2d :bind
  "set_debug_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-server-2d+get-debug-enabled :class 'navigation-server-2d :bind
  "get_debug_enabled" :hash 36873697)
 bool)

(defgmethod
 (navigation-server-2d+get-process-info :class 'navigation-server-2d :bind
  "get_process_info" :hash 1640219858)
 int (process-info navigation-server-2d+process-info))