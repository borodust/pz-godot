(common-lisp:in-package :%godot)


(defgmethod
 (navigation-path-query-parameters-3d+set-pathfinding-algorithm :class
  'navigation-path-query-parameters-3d :bind "set_pathfinding_algorithm" :hash
  394560454)
 :void
 (pathfinding-algorithm
  navigation-path-query-parameters-3d+pathfinding-algorithm))

(defgmethod
 (navigation-path-query-parameters-3d+get-pathfinding-algorithm :class
  'navigation-path-query-parameters-3d :bind "get_pathfinding_algorithm" :hash
  3398491350)
 navigation-path-query-parameters-3d+pathfinding-algorithm)

(defgmethod
 (navigation-path-query-parameters-3d+set-path-postprocessing :class
  'navigation-path-query-parameters-3d :bind "set_path_postprocessing" :hash
  2267362344)
 :void
 (path-postprocessing navigation-path-query-parameters-3d+path-post-processing))

(defgmethod
 (navigation-path-query-parameters-3d+get-path-postprocessing :class
  'navigation-path-query-parameters-3d :bind "get_path_postprocessing" :hash
  3883858360)
 navigation-path-query-parameters-3d+path-post-processing)

(defgmethod
 (navigation-path-query-parameters-3d+set-map :class
  'navigation-path-query-parameters-3d :bind "set_map" :hash 2722037293)
 :void (map rid))

(defgmethod
 (navigation-path-query-parameters-3d+get-map :class
  'navigation-path-query-parameters-3d :bind "get_map" :hash 2944877500)
 rid)

(defgmethod
 (navigation-path-query-parameters-3d+set-start-position :class
  'navigation-path-query-parameters-3d :bind "set_start_position" :hash
  3460891852)
 :void (start-position vector-3))

(defgmethod
 (navigation-path-query-parameters-3d+get-start-position :class
  'navigation-path-query-parameters-3d :bind "get_start_position" :hash
  3360562783)
 vector-3)

(defgmethod
 (navigation-path-query-parameters-3d+set-target-position :class
  'navigation-path-query-parameters-3d :bind "set_target_position" :hash
  3460891852)
 :void (target-position vector-3))

(defgmethod
 (navigation-path-query-parameters-3d+get-target-position :class
  'navigation-path-query-parameters-3d :bind "get_target_position" :hash
  3360562783)
 vector-3)

(defgmethod
 (navigation-path-query-parameters-3d+set-navigation-layers :class
  'navigation-path-query-parameters-3d :bind "set_navigation_layers" :hash
  1286410249)
 :void (navigation-layers int))

(defgmethod
 (navigation-path-query-parameters-3d+get-navigation-layers :class
  'navigation-path-query-parameters-3d :bind "get_navigation_layers" :hash
  3905245786)
 int)

(defgmethod
 (navigation-path-query-parameters-3d+set-metadata-flags :class
  'navigation-path-query-parameters-3d :bind "set_metadata_flags" :hash
  2713846708)
 :void (flags navigation-path-query-parameters-3d+path-metadata-flags))

(defgmethod
 (navigation-path-query-parameters-3d+get-metadata-flags :class
  'navigation-path-query-parameters-3d :bind "get_metadata_flags" :hash
  1582332802)
 navigation-path-query-parameters-3d+path-metadata-flags)

(defgmethod
 (navigation-path-query-parameters-3d+set-simplify-path :class
  'navigation-path-query-parameters-3d :bind "set_simplify_path" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (navigation-path-query-parameters-3d+get-simplify-path :class
  'navigation-path-query-parameters-3d :bind "get_simplify_path" :hash
  36873697)
 bool)

(defgmethod
 (navigation-path-query-parameters-3d+set-simplify-epsilon :class
  'navigation-path-query-parameters-3d :bind "set_simplify_epsilon" :hash
  373806689)
 :void (epsilon float))

(defgmethod
 (navigation-path-query-parameters-3d+get-simplify-epsilon :class
  'navigation-path-query-parameters-3d :bind "get_simplify_epsilon" :hash
  1740695150)
 float)

(defgmethod
 (navigation-path-query-parameters-3d+set-included-regions :class
  'navigation-path-query-parameters-3d :bind "set_included_regions" :hash
  381264803)
 :void (regions array))

(defgmethod
 (navigation-path-query-parameters-3d+get-included-regions :class
  'navigation-path-query-parameters-3d :bind "get_included_regions" :hash
  3995934104)
 array)

(defgmethod
 (navigation-path-query-parameters-3d+set-excluded-regions :class
  'navigation-path-query-parameters-3d :bind "set_excluded_regions" :hash
  381264803)
 :void (regions array))

(defgmethod
 (navigation-path-query-parameters-3d+get-excluded-regions :class
  'navigation-path-query-parameters-3d :bind "get_excluded_regions" :hash
  3995934104)
 array)

(defgmethod
 (navigation-path-query-parameters-3d+set-path-return-max-length :class
  'navigation-path-query-parameters-3d :bind "set_path_return_max_length" :hash
  373806689)
 :void (length float))

(defgmethod
 (navigation-path-query-parameters-3d+get-path-return-max-length :class
  'navigation-path-query-parameters-3d :bind "get_path_return_max_length" :hash
  1740695150)
 float)

(defgmethod
 (navigation-path-query-parameters-3d+set-path-return-max-radius :class
  'navigation-path-query-parameters-3d :bind "set_path_return_max_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (navigation-path-query-parameters-3d+get-path-return-max-radius :class
  'navigation-path-query-parameters-3d :bind "get_path_return_max_radius" :hash
  1740695150)
 float)

(defgmethod
 (navigation-path-query-parameters-3d+set-path-search-max-polygons :class
  'navigation-path-query-parameters-3d :bind "set_path_search_max_polygons"
  :hash 1286410249)
 :void (max-polygons int))

(defgmethod
 (navigation-path-query-parameters-3d+get-path-search-max-polygons :class
  'navigation-path-query-parameters-3d :bind "get_path_search_max_polygons"
  :hash 3905245786)
 int)

(defgmethod
 (navigation-path-query-parameters-3d+set-path-search-max-distance :class
  'navigation-path-query-parameters-3d :bind "set_path_search_max_distance"
  :hash 373806689)
 :void (distance float))

(defgmethod
 (navigation-path-query-parameters-3d+get-path-search-max-distance :class
  'navigation-path-query-parameters-3d :bind "get_path_search_max_distance"
  :hash 1740695150)
 float)