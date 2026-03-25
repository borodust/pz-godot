(common-lisp:in-package :%godot)


(defgmethod
 (navigation-server-3d+get-maps :class 'navigation-server-3d :bind "get_maps"
  :hash 3995934104)
 array)

(defgmethod
 (navigation-server-3d+map-create :class 'navigation-server-3d :bind
  "map_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-3d+map-set-active :class 'navigation-server-3d :bind
  "map_set_active" :hash 1265174801)
 :void (map rid) (active bool))

(defgmethod
 (navigation-server-3d+map-is-active :class 'navigation-server-3d :bind
  "map_is_active" :hash 4155700596)
 bool (map rid))

(defgmethod
 (navigation-server-3d+map-set-up :class 'navigation-server-3d :bind
  "map_set_up" :hash 3227306858)
 :void (map rid) (up vector-3))

(defgmethod
 (navigation-server-3d+map-get-up :class 'navigation-server-3d :bind
  "map_get_up" :hash 531438156)
 vector-3 (map rid))

(defgmethod
 (navigation-server-3d+map-set-cell-size :class 'navigation-server-3d :bind
  "map_set_cell_size" :hash 1794382983)
 :void (map rid) (cell-size float))

(defgmethod
 (navigation-server-3d+map-get-cell-size :class 'navigation-server-3d :bind
  "map_get_cell_size" :hash 866169185)
 float (map rid))

(defgmethod
 (navigation-server-3d+map-set-cell-height :class 'navigation-server-3d :bind
  "map_set_cell_height" :hash 1794382983)
 :void (map rid) (cell-height float))

(defgmethod
 (navigation-server-3d+map-get-cell-height :class 'navigation-server-3d :bind
  "map_get_cell_height" :hash 866169185)
 float (map rid))

(defgmethod
 (navigation-server-3d+map-set-merge-rasterizer-cell-scale :class
  'navigation-server-3d :bind "map_set_merge_rasterizer_cell_scale" :hash
  1794382983)
 :void (map rid) (scale float))

(defgmethod
 (navigation-server-3d+map-get-merge-rasterizer-cell-scale :class
  'navigation-server-3d :bind "map_get_merge_rasterizer_cell_scale" :hash
  866169185)
 float (map rid))

(defgmethod
 (navigation-server-3d+map-set-use-edge-connections :class
  'navigation-server-3d :bind "map_set_use_edge_connections" :hash 1265174801)
 :void (map rid) (enabled bool))

(defgmethod
 (navigation-server-3d+map-get-use-edge-connections :class
  'navigation-server-3d :bind "map_get_use_edge_connections" :hash 4155700596)
 bool (map rid))

(defgmethod
 (navigation-server-3d+map-set-edge-connection-margin :class
  'navigation-server-3d :bind "map_set_edge_connection_margin" :hash
  1794382983)
 :void (map rid) (margin float))

(defgmethod
 (navigation-server-3d+map-get-edge-connection-margin :class
  'navigation-server-3d :bind "map_get_edge_connection_margin" :hash 866169185)
 float (map rid))

(defgmethod
 (navigation-server-3d+map-set-link-connection-radius :class
  'navigation-server-3d :bind "map_set_link_connection_radius" :hash
  1794382983)
 :void (map rid) (radius float))

(defgmethod
 (navigation-server-3d+map-get-link-connection-radius :class
  'navigation-server-3d :bind "map_get_link_connection_radius" :hash 866169185)
 float (map rid))

(defgmethod
 (navigation-server-3d+map-get-path :class 'navigation-server-3d :bind
  "map_get_path" :hash 276783190)
 packed-vector-3array (map rid) (origin vector-3) (destination vector-3)
 (optimize bool) (navigation-layers int))

(defgmethod
 (navigation-server-3d+map-get-closest-point-to-segment :class
  'navigation-server-3d :bind "map_get_closest_point_to_segment" :hash
  3830095642)
 vector-3 (map rid) (start vector-3) (end vector-3) (use-collision bool))

(defgmethod
 (navigation-server-3d+map-get-closest-point :class 'navigation-server-3d :bind
  "map_get_closest_point" :hash 2056183332)
 vector-3 (map rid) (to-point vector-3))

(defgmethod
 (navigation-server-3d+map-get-closest-point-normal :class
  'navigation-server-3d :bind "map_get_closest_point_normal" :hash 2056183332)
 vector-3 (map rid) (to-point vector-3))

(defgmethod
 (navigation-server-3d+map-get-closest-point-owner :class 'navigation-server-3d
  :bind "map_get_closest_point_owner" :hash 553364610)
 rid (map rid) (to-point vector-3))

(defgmethod
 (navigation-server-3d+map-get-links :class 'navigation-server-3d :bind
  "map_get_links" :hash 2684255073)
 array (map rid))

(defgmethod
 (navigation-server-3d+map-get-regions :class 'navigation-server-3d :bind
  "map_get_regions" :hash 2684255073)
 array (map rid))

(defgmethod
 (navigation-server-3d+map-get-agents :class 'navigation-server-3d :bind
  "map_get_agents" :hash 2684255073)
 array (map rid))

(defgmethod
 (navigation-server-3d+map-get-obstacles :class 'navigation-server-3d :bind
  "map_get_obstacles" :hash 2684255073)
 array (map rid))

(defgmethod
 (navigation-server-3d+map-force-update :class 'navigation-server-3d :bind
  "map_force_update" :hash 2722037293)
 :void (map rid))

(defgmethod
 (navigation-server-3d+map-get-iteration-id :class 'navigation-server-3d :bind
  "map_get_iteration_id" :hash 2198884583)
 int (map rid))

(defgmethod
 (navigation-server-3d+map-set-use-async-iterations :class
  'navigation-server-3d :bind "map_set_use_async_iterations" :hash 1265174801)
 :void (map rid) (enabled bool))

(defgmethod
 (navigation-server-3d+map-get-use-async-iterations :class
  'navigation-server-3d :bind "map_get_use_async_iterations" :hash 4155700596)
 bool (map rid))

(defgmethod
 (navigation-server-3d+map-get-random-point :class 'navigation-server-3d :bind
  "map_get_random_point" :hash 722801526)
 vector-3 (map rid) (navigation-layers int) (uniformly bool))

(defgmethod
 (navigation-server-3d+query-path :class 'navigation-server-3d :bind
  "query_path" :hash 2146930868)
 :void (parameters navigation-path-query-parameters-3d)
 (result navigation-path-query-result-3d) (callback callable))

(defgmethod
 (navigation-server-3d+region-create :class 'navigation-server-3d :bind
  "region_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-3d+region-get-iteration-id :class 'navigation-server-3d
  :bind "region_get_iteration_id" :hash 2198884583)
 int (region rid))

(defgmethod
 (navigation-server-3d+region-set-use-async-iterations :class
  'navigation-server-3d :bind "region_set_use_async_iterations" :hash
  1265174801)
 :void (region rid) (enabled bool))

(defgmethod
 (navigation-server-3d+region-get-use-async-iterations :class
  'navigation-server-3d :bind "region_get_use_async_iterations" :hash
  4155700596)
 bool (region rid))

(defgmethod
 (navigation-server-3d+region-set-enabled :class 'navigation-server-3d :bind
  "region_set_enabled" :hash 1265174801)
 :void (region rid) (enabled bool))

(defgmethod
 (navigation-server-3d+region-get-enabled :class 'navigation-server-3d :bind
  "region_get_enabled" :hash 4155700596)
 bool (region rid))

(defgmethod
 (navigation-server-3d+region-set-use-edge-connections :class
  'navigation-server-3d :bind "region_set_use_edge_connections" :hash
  1265174801)
 :void (region rid) (enabled bool))

(defgmethod
 (navigation-server-3d+region-get-use-edge-connections :class
  'navigation-server-3d :bind "region_get_use_edge_connections" :hash
  4155700596)
 bool (region rid))

(defgmethod
 (navigation-server-3d+region-set-enter-cost :class 'navigation-server-3d :bind
  "region_set_enter_cost" :hash 1794382983)
 :void (region rid) (enter-cost float))

(defgmethod
 (navigation-server-3d+region-get-enter-cost :class 'navigation-server-3d :bind
  "region_get_enter_cost" :hash 866169185)
 float (region rid))

(defgmethod
 (navigation-server-3d+region-set-travel-cost :class 'navigation-server-3d
  :bind "region_set_travel_cost" :hash 1794382983)
 :void (region rid) (travel-cost float))

(defgmethod
 (navigation-server-3d+region-get-travel-cost :class 'navigation-server-3d
  :bind "region_get_travel_cost" :hash 866169185)
 float (region rid))

(defgmethod
 (navigation-server-3d+region-set-owner-id :class 'navigation-server-3d :bind
  "region_set_owner_id" :hash 3411492887)
 :void (region rid) (owner-id int))

(defgmethod
 (navigation-server-3d+region-get-owner-id :class 'navigation-server-3d :bind
  "region_get_owner_id" :hash 2198884583)
 int (region rid))

(defgmethod
 (navigation-server-3d+region-owns-point :class 'navigation-server-3d :bind
  "region_owns_point" :hash 2360011153)
 bool (region rid) (point vector-3))

(defgmethod
 (navigation-server-3d+region-set-map :class 'navigation-server-3d :bind
  "region_set_map" :hash 395945892)
 :void (region rid) (map rid))

(defgmethod
 (navigation-server-3d+region-get-map :class 'navigation-server-3d :bind
  "region_get_map" :hash 3814569979)
 rid (region rid))

(defgmethod
 (navigation-server-3d+region-set-navigation-layers :class
  'navigation-server-3d :bind "region_set_navigation_layers" :hash 3411492887)
 :void (region rid) (navigation-layers int))

(defgmethod
 (navigation-server-3d+region-get-navigation-layers :class
  'navigation-server-3d :bind "region_get_navigation_layers" :hash 2198884583)
 int (region rid))

(defgmethod
 (navigation-server-3d+region-set-transform :class 'navigation-server-3d :bind
  "region_set_transform" :hash 3935195649)
 :void (region rid) (transform transform-3d))

(defgmethod
 (navigation-server-3d+region-get-transform :class 'navigation-server-3d :bind
  "region_get_transform" :hash 1128465797)
 transform-3d (region rid))

(defgmethod
 (navigation-server-3d+region-set-navigation-mesh :class 'navigation-server-3d
  :bind "region_set_navigation_mesh" :hash 2764952978)
 :void (region rid) (navigation-mesh navigation-mesh))

(defgmethod
 (navigation-server-3d+region-bake-navigation-mesh :class 'navigation-server-3d
  :bind "region_bake_navigation_mesh" :hash 1401173477)
 :void (navigation-mesh navigation-mesh) (root-node node))

(defgmethod
 (navigation-server-3d+region-get-connections-count :class
  'navigation-server-3d :bind "region_get_connections_count" :hash 2198884583)
 int (region rid))

(defgmethod
 (navigation-server-3d+region-get-connection-pathway-start :class
  'navigation-server-3d :bind "region_get_connection_pathway_start" :hash
  3440143363)
 vector-3 (region rid) (connection int))

(defgmethod
 (navigation-server-3d+region-get-connection-pathway-end :class
  'navigation-server-3d :bind "region_get_connection_pathway_end" :hash
  3440143363)
 vector-3 (region rid) (connection int))

(defgmethod
 (navigation-server-3d+region-get-closest-point-to-segment :class
  'navigation-server-3d :bind "region_get_closest_point_to_segment" :hash
  3830095642)
 vector-3 (region rid) (start vector-3) (end vector-3) (use-collision bool))

(defgmethod
 (navigation-server-3d+region-get-closest-point :class 'navigation-server-3d
  :bind "region_get_closest_point" :hash 2056183332)
 vector-3 (region rid) (to-point vector-3))

(defgmethod
 (navigation-server-3d+region-get-closest-point-normal :class
  'navigation-server-3d :bind "region_get_closest_point_normal" :hash
  2056183332)
 vector-3 (region rid) (to-point vector-3))

(defgmethod
 (navigation-server-3d+region-get-random-point :class 'navigation-server-3d
  :bind "region_get_random_point" :hash 722801526)
 vector-3 (region rid) (navigation-layers int) (uniformly bool))

(defgmethod
 (navigation-server-3d+region-get-bounds :class 'navigation-server-3d :bind
  "region_get_bounds" :hash 974181306)
 aabb (region rid))

(defgmethod
 (navigation-server-3d+link-create :class 'navigation-server-3d :bind
  "link_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-3d+link-get-iteration-id :class 'navigation-server-3d :bind
  "link_get_iteration_id" :hash 2198884583)
 int (link rid))

(defgmethod
 (navigation-server-3d+link-set-map :class 'navigation-server-3d :bind
  "link_set_map" :hash 395945892)
 :void (link rid) (map rid))

(defgmethod
 (navigation-server-3d+link-get-map :class 'navigation-server-3d :bind
  "link_get_map" :hash 3814569979)
 rid (link rid))

(defgmethod
 (navigation-server-3d+link-set-enabled :class 'navigation-server-3d :bind
  "link_set_enabled" :hash 1265174801)
 :void (link rid) (enabled bool))

(defgmethod
 (navigation-server-3d+link-get-enabled :class 'navigation-server-3d :bind
  "link_get_enabled" :hash 4155700596)
 bool (link rid))

(defgmethod
 (navigation-server-3d+link-set-bidirectional :class 'navigation-server-3d
  :bind "link_set_bidirectional" :hash 1265174801)
 :void (link rid) (bidirectional bool))

(defgmethod
 (navigation-server-3d+link-is-bidirectional :class 'navigation-server-3d :bind
  "link_is_bidirectional" :hash 4155700596)
 bool (link rid))

(defgmethod
 (navigation-server-3d+link-set-navigation-layers :class 'navigation-server-3d
  :bind "link_set_navigation_layers" :hash 3411492887)
 :void (link rid) (navigation-layers int))

(defgmethod
 (navigation-server-3d+link-get-navigation-layers :class 'navigation-server-3d
  :bind "link_get_navigation_layers" :hash 2198884583)
 int (link rid))

(defgmethod
 (navigation-server-3d+link-set-start-position :class 'navigation-server-3d
  :bind "link_set_start_position" :hash 3227306858)
 :void (link rid) (position vector-3))

(defgmethod
 (navigation-server-3d+link-get-start-position :class 'navigation-server-3d
  :bind "link_get_start_position" :hash 531438156)
 vector-3 (link rid))

(defgmethod
 (navigation-server-3d+link-set-end-position :class 'navigation-server-3d :bind
  "link_set_end_position" :hash 3227306858)
 :void (link rid) (position vector-3))

(defgmethod
 (navigation-server-3d+link-get-end-position :class 'navigation-server-3d :bind
  "link_get_end_position" :hash 531438156)
 vector-3 (link rid))

(defgmethod
 (navigation-server-3d+link-set-enter-cost :class 'navigation-server-3d :bind
  "link_set_enter_cost" :hash 1794382983)
 :void (link rid) (enter-cost float))

(defgmethod
 (navigation-server-3d+link-get-enter-cost :class 'navigation-server-3d :bind
  "link_get_enter_cost" :hash 866169185)
 float (link rid))

(defgmethod
 (navigation-server-3d+link-set-travel-cost :class 'navigation-server-3d :bind
  "link_set_travel_cost" :hash 1794382983)
 :void (link rid) (travel-cost float))

(defgmethod
 (navigation-server-3d+link-get-travel-cost :class 'navigation-server-3d :bind
  "link_get_travel_cost" :hash 866169185)
 float (link rid))

(defgmethod
 (navigation-server-3d+link-set-owner-id :class 'navigation-server-3d :bind
  "link_set_owner_id" :hash 3411492887)
 :void (link rid) (owner-id int))

(defgmethod
 (navigation-server-3d+link-get-owner-id :class 'navigation-server-3d :bind
  "link_get_owner_id" :hash 2198884583)
 int (link rid))

(defgmethod
 (navigation-server-3d+agent-create :class 'navigation-server-3d :bind
  "agent_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-3d+agent-set-avoidance-enabled :class 'navigation-server-3d
  :bind "agent_set_avoidance_enabled" :hash 1265174801)
 :void (agent rid) (enabled bool))

(defgmethod
 (navigation-server-3d+agent-get-avoidance-enabled :class 'navigation-server-3d
  :bind "agent_get_avoidance_enabled" :hash 4155700596)
 bool (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-use-3d-avoidance :class 'navigation-server-3d
  :bind "agent_set_use_3d_avoidance" :hash 1265174801)
 :void (agent rid) (enabled bool))

(defgmethod
 (navigation-server-3d+agent-get-use-3d-avoidance :class 'navigation-server-3d
  :bind "agent_get_use_3d_avoidance" :hash 4155700596)
 bool (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-map :class 'navigation-server-3d :bind
  "agent_set_map" :hash 395945892)
 :void (agent rid) (map rid))

(defgmethod
 (navigation-server-3d+agent-get-map :class 'navigation-server-3d :bind
  "agent_get_map" :hash 3814569979)
 rid (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-paused :class 'navigation-server-3d :bind
  "agent_set_paused" :hash 1265174801)
 :void (agent rid) (paused bool))

(defgmethod
 (navigation-server-3d+agent-get-paused :class 'navigation-server-3d :bind
  "agent_get_paused" :hash 4155700596)
 bool (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-neighbor-distance :class 'navigation-server-3d
  :bind "agent_set_neighbor_distance" :hash 1794382983)
 :void (agent rid) (distance float))

(defgmethod
 (navigation-server-3d+agent-get-neighbor-distance :class 'navigation-server-3d
  :bind "agent_get_neighbor_distance" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-max-neighbors :class 'navigation-server-3d
  :bind "agent_set_max_neighbors" :hash 3411492887)
 :void (agent rid) (count int))

(defgmethod
 (navigation-server-3d+agent-get-max-neighbors :class 'navigation-server-3d
  :bind "agent_get_max_neighbors" :hash 2198884583)
 int (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-time-horizon-agents :class
  'navigation-server-3d :bind "agent_set_time_horizon_agents" :hash 1794382983)
 :void (agent rid) (time-horizon float))

(defgmethod
 (navigation-server-3d+agent-get-time-horizon-agents :class
  'navigation-server-3d :bind "agent_get_time_horizon_agents" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-time-horizon-obstacles :class
  'navigation-server-3d :bind "agent_set_time_horizon_obstacles" :hash
  1794382983)
 :void (agent rid) (time-horizon float))

(defgmethod
 (navigation-server-3d+agent-get-time-horizon-obstacles :class
  'navigation-server-3d :bind "agent_get_time_horizon_obstacles" :hash
  866169185)
 float (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-radius :class 'navigation-server-3d :bind
  "agent_set_radius" :hash 1794382983)
 :void (agent rid) (radius float))

(defgmethod
 (navigation-server-3d+agent-get-radius :class 'navigation-server-3d :bind
  "agent_get_radius" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-height :class 'navigation-server-3d :bind
  "agent_set_height" :hash 1794382983)
 :void (agent rid) (height float))

(defgmethod
 (navigation-server-3d+agent-get-height :class 'navigation-server-3d :bind
  "agent_get_height" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-max-speed :class 'navigation-server-3d :bind
  "agent_set_max_speed" :hash 1794382983)
 :void (agent rid) (max-speed float))

(defgmethod
 (navigation-server-3d+agent-get-max-speed :class 'navigation-server-3d :bind
  "agent_get_max_speed" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-velocity-forced :class 'navigation-server-3d
  :bind "agent_set_velocity_forced" :hash 3227306858)
 :void (agent rid) (velocity vector-3))

(defgmethod
 (navigation-server-3d+agent-set-velocity :class 'navigation-server-3d :bind
  "agent_set_velocity" :hash 3227306858)
 :void (agent rid) (velocity vector-3))

(defgmethod
 (navigation-server-3d+agent-get-velocity :class 'navigation-server-3d :bind
  "agent_get_velocity" :hash 531438156)
 vector-3 (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-position :class 'navigation-server-3d :bind
  "agent_set_position" :hash 3227306858)
 :void (agent rid) (position vector-3))

(defgmethod
 (navigation-server-3d+agent-get-position :class 'navigation-server-3d :bind
  "agent_get_position" :hash 531438156)
 vector-3 (agent rid))

(defgmethod
 (navigation-server-3d+agent-is-map-changed :class 'navigation-server-3d :bind
  "agent_is_map_changed" :hash 4155700596)
 bool (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-avoidance-callback :class
  'navigation-server-3d :bind "agent_set_avoidance_callback" :hash 3379118538)
 :void (agent rid) (callback callable))

(defgmethod
 (navigation-server-3d+agent-has-avoidance-callback :class
  'navigation-server-3d :bind "agent_has_avoidance_callback" :hash 4155700596)
 bool (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-avoidance-layers :class 'navigation-server-3d
  :bind "agent_set_avoidance_layers" :hash 3411492887)
 :void (agent rid) (layers int))

(defgmethod
 (navigation-server-3d+agent-get-avoidance-layers :class 'navigation-server-3d
  :bind "agent_get_avoidance_layers" :hash 2198884583)
 int (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-avoidance-mask :class 'navigation-server-3d
  :bind "agent_set_avoidance_mask" :hash 3411492887)
 :void (agent rid) (mask int))

(defgmethod
 (navigation-server-3d+agent-get-avoidance-mask :class 'navigation-server-3d
  :bind "agent_get_avoidance_mask" :hash 2198884583)
 int (agent rid))

(defgmethod
 (navigation-server-3d+agent-set-avoidance-priority :class
  'navigation-server-3d :bind "agent_set_avoidance_priority" :hash 1794382983)
 :void (agent rid) (priority float))

(defgmethod
 (navigation-server-3d+agent-get-avoidance-priority :class
  'navigation-server-3d :bind "agent_get_avoidance_priority" :hash 866169185)
 float (agent rid))

(defgmethod
 (navigation-server-3d+obstacle-create :class 'navigation-server-3d :bind
  "obstacle_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-3d+obstacle-set-avoidance-enabled :class
  'navigation-server-3d :bind "obstacle_set_avoidance_enabled" :hash
  1265174801)
 :void (obstacle rid) (enabled bool))

(defgmethod
 (navigation-server-3d+obstacle-get-avoidance-enabled :class
  'navigation-server-3d :bind "obstacle_get_avoidance_enabled" :hash
  4155700596)
 bool (obstacle rid))

(defgmethod
 (navigation-server-3d+obstacle-set-use-3d-avoidance :class
  'navigation-server-3d :bind "obstacle_set_use_3d_avoidance" :hash 1265174801)
 :void (obstacle rid) (enabled bool))

(defgmethod
 (navigation-server-3d+obstacle-get-use-3d-avoidance :class
  'navigation-server-3d :bind "obstacle_get_use_3d_avoidance" :hash 4155700596)
 bool (obstacle rid))

(defgmethod
 (navigation-server-3d+obstacle-set-map :class 'navigation-server-3d :bind
  "obstacle_set_map" :hash 395945892)
 :void (obstacle rid) (map rid))

(defgmethod
 (navigation-server-3d+obstacle-get-map :class 'navigation-server-3d :bind
  "obstacle_get_map" :hash 3814569979)
 rid (obstacle rid))

(defgmethod
 (navigation-server-3d+obstacle-set-paused :class 'navigation-server-3d :bind
  "obstacle_set_paused" :hash 1265174801)
 :void (obstacle rid) (paused bool))

(defgmethod
 (navigation-server-3d+obstacle-get-paused :class 'navigation-server-3d :bind
  "obstacle_get_paused" :hash 4155700596)
 bool (obstacle rid))

(defgmethod
 (navigation-server-3d+obstacle-set-radius :class 'navigation-server-3d :bind
  "obstacle_set_radius" :hash 1794382983)
 :void (obstacle rid) (radius float))

(defgmethod
 (navigation-server-3d+obstacle-get-radius :class 'navigation-server-3d :bind
  "obstacle_get_radius" :hash 866169185)
 float (obstacle rid))

(defgmethod
 (navigation-server-3d+obstacle-set-height :class 'navigation-server-3d :bind
  "obstacle_set_height" :hash 1794382983)
 :void (obstacle rid) (height float))

(defgmethod
 (navigation-server-3d+obstacle-get-height :class 'navigation-server-3d :bind
  "obstacle_get_height" :hash 866169185)
 float (obstacle rid))

(defgmethod
 (navigation-server-3d+obstacle-set-velocity :class 'navigation-server-3d :bind
  "obstacle_set_velocity" :hash 3227306858)
 :void (obstacle rid) (velocity vector-3))

(defgmethod
 (navigation-server-3d+obstacle-get-velocity :class 'navigation-server-3d :bind
  "obstacle_get_velocity" :hash 531438156)
 vector-3 (obstacle rid))

(defgmethod
 (navigation-server-3d+obstacle-set-position :class 'navigation-server-3d :bind
  "obstacle_set_position" :hash 3227306858)
 :void (obstacle rid) (position vector-3))

(defgmethod
 (navigation-server-3d+obstacle-get-position :class 'navigation-server-3d :bind
  "obstacle_get_position" :hash 531438156)
 vector-3 (obstacle rid))

(defgmethod
 (navigation-server-3d+obstacle-set-vertices :class 'navigation-server-3d :bind
  "obstacle_set_vertices" :hash 4030257846)
 :void (obstacle rid) (vertices packed-vector-3array))

(defgmethod
 (navigation-server-3d+obstacle-get-vertices :class 'navigation-server-3d :bind
  "obstacle_get_vertices" :hash 808965560)
 packed-vector-3array (obstacle rid))

(defgmethod
 (navigation-server-3d+obstacle-set-avoidance-layers :class
  'navigation-server-3d :bind "obstacle_set_avoidance_layers" :hash 3411492887)
 :void (obstacle rid) (layers int))

(defgmethod
 (navigation-server-3d+obstacle-get-avoidance-layers :class
  'navigation-server-3d :bind "obstacle_get_avoidance_layers" :hash 2198884583)
 int (obstacle rid))

(defgmethod
 (navigation-server-3d+parse-source-geometry-data :class 'navigation-server-3d
  :bind "parse_source_geometry_data" :hash 3172802542)
 :void (navigation-mesh navigation-mesh)
 (source-geometry-data navigation-mesh-source-geometry-data-3d)
 (root-node node) (callback callable))

(defgmethod
 (navigation-server-3d+bake-from-source-geometry-data :class
  'navigation-server-3d :bind "bake_from_source_geometry_data" :hash
  1286748856)
 :void (navigation-mesh navigation-mesh)
 (source-geometry-data navigation-mesh-source-geometry-data-3d)
 (callback callable))

(defgmethod
 (navigation-server-3d+bake-from-source-geometry-data-async :class
  'navigation-server-3d :bind "bake_from_source_geometry_data_async" :hash
  1286748856)
 :void (navigation-mesh navigation-mesh)
 (source-geometry-data navigation-mesh-source-geometry-data-3d)
 (callback callable))

(defgmethod
 (navigation-server-3d+is-baking-navigation-mesh :class 'navigation-server-3d
  :bind "is_baking_navigation_mesh" :hash 3142026141)
 bool (navigation-mesh navigation-mesh))

(defgmethod
 (navigation-server-3d+source-geometry-parser-create :class
  'navigation-server-3d :bind "source_geometry_parser_create" :hash 529393457)
 rid)

(defgmethod
 (navigation-server-3d+source-geometry-parser-set-callback :class
  'navigation-server-3d :bind "source_geometry_parser_set_callback" :hash
  3379118538)
 :void (parser rid) (callback callable))

(defgmethod
 (navigation-server-3d+simplify-path :class 'navigation-server-3d :bind
  "simplify_path" :hash 2344122170)
 packed-vector-3array (path packed-vector-3array) (epsilon float))

(defgmethod
 (navigation-server-3d+free-rid :class 'navigation-server-3d :bind "free_rid"
  :hash 2722037293)
 :void (rid rid))

(defgmethod
 (navigation-server-3d+set-active :class 'navigation-server-3d :bind
  "set_active" :hash 2586408642)
 :void (active bool))

(defgmethod
 (navigation-server-3d+set-debug-enabled :class 'navigation-server-3d :bind
  "set_debug_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-server-3d+get-debug-enabled :class 'navigation-server-3d :bind
  "get_debug_enabled" :hash 36873697)
 bool)

(defgmethod
 (navigation-server-3d+get-process-info :class 'navigation-server-3d :bind
  "get_process_info" :hash 1938440894)
 int (process-info navigation-server-3d+process-info))