(common-lisp:in-package :%godot)


(defgmethod
 (navigation-mesh+set-sample-partition-type :class 'navigation-mesh :bind
  "set_sample_partition_type" :hash 2472437533)
 :void (sample-partition-type navigation-mesh+sample-partition-type))

(defgmethod
 (navigation-mesh+get-sample-partition-type :class 'navigation-mesh :bind
  "get_sample_partition_type" :hash 833513918)
 navigation-mesh+sample-partition-type)

(defgmethod
 (navigation-mesh+set-parsed-geometry-type :class 'navigation-mesh :bind
  "set_parsed_geometry_type" :hash 3064713163)
 :void (geometry-type navigation-mesh+parsed-geometry-type))

(defgmethod
 (navigation-mesh+get-parsed-geometry-type :class 'navigation-mesh :bind
  "get_parsed_geometry_type" :hash 3928011953)
 navigation-mesh+parsed-geometry-type)

(defgmethod
 (navigation-mesh+set-collision-mask :class 'navigation-mesh :bind
  "set_collision_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (navigation-mesh+get-collision-mask :class 'navigation-mesh :bind
  "get_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (navigation-mesh+set-collision-mask-value :class 'navigation-mesh :bind
  "set_collision_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (navigation-mesh+get-collision-mask-value :class 'navigation-mesh :bind
  "get_collision_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (navigation-mesh+set-source-geometry-mode :class 'navigation-mesh :bind
  "set_source_geometry_mode" :hash 2700825194)
 :void (mask navigation-mesh+source-geometry-mode))

(defgmethod
 (navigation-mesh+get-source-geometry-mode :class 'navigation-mesh :bind
  "get_source_geometry_mode" :hash 2770484141)
 navigation-mesh+source-geometry-mode)

(defgmethod
 (navigation-mesh+set-source-group-name :class 'navigation-mesh :bind
  "set_source_group_name" :hash 3304788590)
 :void (mask string-name))

(defgmethod
 (navigation-mesh+get-source-group-name :class 'navigation-mesh :bind
  "get_source_group_name" :hash 2002593661)
 string-name)

(defgmethod
 (navigation-mesh+set-cell-size :class 'navigation-mesh :bind "set_cell_size"
  :hash 373806689)
 :void (cell-size float))

(defgmethod
 (navigation-mesh+get-cell-size :class 'navigation-mesh :bind "get_cell_size"
  :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-cell-height :class 'navigation-mesh :bind
  "set_cell_height" :hash 373806689)
 :void (cell-height float))

(defgmethod
 (navigation-mesh+get-cell-height :class 'navigation-mesh :bind
  "get_cell_height" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-border-size :class 'navigation-mesh :bind
  "set_border_size" :hash 373806689)
 :void (border-size float))

(defgmethod
 (navigation-mesh+get-border-size :class 'navigation-mesh :bind
  "get_border_size" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-agent-height :class 'navigation-mesh :bind
  "set_agent_height" :hash 373806689)
 :void (agent-height float))

(defgmethod
 (navigation-mesh+get-agent-height :class 'navigation-mesh :bind
  "get_agent_height" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-agent-radius :class 'navigation-mesh :bind
  "set_agent_radius" :hash 373806689)
 :void (agent-radius float))

(defgmethod
 (navigation-mesh+get-agent-radius :class 'navigation-mesh :bind
  "get_agent_radius" :hash 191475506)
 float)

(defgmethod
 (navigation-mesh+set-agent-max-climb :class 'navigation-mesh :bind
  "set_agent_max_climb" :hash 373806689)
 :void (agent-max-climb float))

(defgmethod
 (navigation-mesh+get-agent-max-climb :class 'navigation-mesh :bind
  "get_agent_max_climb" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-agent-max-slope :class 'navigation-mesh :bind
  "set_agent_max_slope" :hash 373806689)
 :void (agent-max-slope float))

(defgmethod
 (navigation-mesh+get-agent-max-slope :class 'navigation-mesh :bind
  "get_agent_max_slope" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-region-min-size :class 'navigation-mesh :bind
  "set_region_min_size" :hash 373806689)
 :void (region-min-size float))

(defgmethod
 (navigation-mesh+get-region-min-size :class 'navigation-mesh :bind
  "get_region_min_size" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-region-merge-size :class 'navigation-mesh :bind
  "set_region_merge_size" :hash 373806689)
 :void (region-merge-size float))

(defgmethod
 (navigation-mesh+get-region-merge-size :class 'navigation-mesh :bind
  "get_region_merge_size" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-edge-max-length :class 'navigation-mesh :bind
  "set_edge_max_length" :hash 373806689)
 :void (edge-max-length float))

(defgmethod
 (navigation-mesh+get-edge-max-length :class 'navigation-mesh :bind
  "get_edge_max_length" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-edge-max-error :class 'navigation-mesh :bind
  "set_edge_max_error" :hash 373806689)
 :void (edge-max-error float))

(defgmethod
 (navigation-mesh+get-edge-max-error :class 'navigation-mesh :bind
  "get_edge_max_error" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-vertices-per-polygon :class 'navigation-mesh :bind
  "set_vertices_per_polygon" :hash 373806689)
 :void (vertices-per-polygon float))

(defgmethod
 (navigation-mesh+get-vertices-per-polygon :class 'navigation-mesh :bind
  "get_vertices_per_polygon" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-detail-sample-distance :class 'navigation-mesh :bind
  "set_detail_sample_distance" :hash 373806689)
 :void (detail-sample-dist float))

(defgmethod
 (navigation-mesh+get-detail-sample-distance :class 'navigation-mesh :bind
  "get_detail_sample_distance" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-detail-sample-max-error :class 'navigation-mesh :bind
  "set_detail_sample_max_error" :hash 373806689)
 :void (detail-sample-max-error float))

(defgmethod
 (navigation-mesh+get-detail-sample-max-error :class 'navigation-mesh :bind
  "get_detail_sample_max_error" :hash 1740695150)
 float)

(defgmethod
 (navigation-mesh+set-filter-low-hanging-obstacles :class 'navigation-mesh
  :bind "set_filter_low_hanging_obstacles" :hash 2586408642)
 :void (filter-low-hanging-obstacles bool))

(defgmethod
 (navigation-mesh+get-filter-low-hanging-obstacles :class 'navigation-mesh
  :bind "get_filter_low_hanging_obstacles" :hash 36873697)
 bool)

(defgmethod
 (navigation-mesh+set-filter-ledge-spans :class 'navigation-mesh :bind
  "set_filter_ledge_spans" :hash 2586408642)
 :void (filter-ledge-spans bool))

(defgmethod
 (navigation-mesh+get-filter-ledge-spans :class 'navigation-mesh :bind
  "get_filter_ledge_spans" :hash 36873697)
 bool)

(defgmethod
 (navigation-mesh+set-filter-walkable-low-height-spans :class 'navigation-mesh
  :bind "set_filter_walkable_low_height_spans" :hash 2586408642)
 :void (filter-walkable-low-height-spans bool))

(defgmethod
 (navigation-mesh+get-filter-walkable-low-height-spans :class 'navigation-mesh
  :bind "get_filter_walkable_low_height_spans" :hash 36873697)
 bool)

(defgmethod
 (navigation-mesh+set-filter-baking-aabb :class 'navigation-mesh :bind
  "set_filter_baking_aabb" :hash 259215842)
 :void (baking-aabb aabb))

(defgmethod
 (navigation-mesh+get-filter-baking-aabb :class 'navigation-mesh :bind
  "get_filter_baking_aabb" :hash 1068685055)
 aabb)

(defgmethod
 (navigation-mesh+set-filter-baking-aabb-offset :class 'navigation-mesh :bind
  "set_filter_baking_aabb_offset" :hash 3460891852)
 :void (baking-aabb-offset vector-3))

(defgmethod
 (navigation-mesh+get-filter-baking-aabb-offset :class 'navigation-mesh :bind
  "get_filter_baking_aabb_offset" :hash 3360562783)
 vector-3)

(defgmethod
 (navigation-mesh+set-vertices :class 'navigation-mesh :bind "set_vertices"
  :hash 334873810)
 :void (vertices packed-vector-3array))

(defgmethod
 (navigation-mesh+get-vertices :class 'navigation-mesh :bind "get_vertices"
  :hash 497664490)
 packed-vector-3array)

(defgmethod
 (navigation-mesh+add-polygon :class 'navigation-mesh :bind "add_polygon" :hash
  3614634198)
 :void (polygon packed-int-32array))

(defgmethod
 (navigation-mesh+get-polygon-count :class 'navigation-mesh :bind
  "get_polygon_count" :hash 3905245786)
 int)

(defgmethod
 (navigation-mesh+get-polygon :class 'navigation-mesh :bind "get_polygon" :hash
  3668444399)
 packed-int-32array (idx int))

(defgmethod
 (navigation-mesh+clear-polygons :class 'navigation-mesh :bind "clear_polygons"
  :hash 3218959716)
 :void)

(defgmethod
 (navigation-mesh+create-from-mesh :class 'navigation-mesh :bind
  "create_from_mesh" :hash 194775623)
 :void (mesh mesh))

(defgmethod
 (navigation-mesh+clear :class 'navigation-mesh :bind "clear" :hash 3218959716)
 :void)