(common-lisp:in-package :%godot)


(defgmethod
 (tile-map-layer+%use-tile-data-runtime-update :class 'tile-map-layer :bind
  "_use_tile_data_runtime_update" :hash 3715736492 :virtual common-lisp:t)
 bool (coords vector-2i))

(defgmethod
 (tile-map-layer+%tile-data-runtime-update :class 'tile-map-layer :bind
  "_tile_data_runtime_update" :hash 1627322126 :virtual common-lisp:t)
 :void (coords vector-2i) (tile-data tile-data))

(defgmethod
 (tile-map-layer+%update-cells :class 'tile-map-layer :bind "_update_cells"
  :hash 3156113851 :virtual common-lisp:t)
 :void (coords array) (forced-cleanup bool))

(defgmethod
 (tile-map-layer+set-cell :class 'tile-map-layer :bind "set_cell" :hash
  2428518503)
 :void (coords vector-2i) (source-id int) (atlas-coords vector-2i)
 (alternative-tile int))

(defgmethod
 (tile-map-layer+erase-cell :class 'tile-map-layer :bind "erase_cell" :hash
  1130785943)
 :void (coords vector-2i))

(defgmethod
 (tile-map-layer+fix-invalid-tiles :class 'tile-map-layer :bind
  "fix_invalid_tiles" :hash 3218959716)
 :void)

(defgmethod
 (tile-map-layer+clear :class 'tile-map-layer :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (tile-map-layer+get-cell-source-id :class 'tile-map-layer :bind
  "get_cell_source_id" :hash 2485466453)
 int (coords vector-2i))

(defgmethod
 (tile-map-layer+get-cell-atlas-coords :class 'tile-map-layer :bind
  "get_cell_atlas_coords" :hash 3050897911)
 vector-2i (coords vector-2i))

(defgmethod
 (tile-map-layer+get-cell-alternative-tile :class 'tile-map-layer :bind
  "get_cell_alternative_tile" :hash 2485466453)
 int (coords vector-2i))

(defgmethod
 (tile-map-layer+get-cell-tile-data :class 'tile-map-layer :bind
  "get_cell_tile_data" :hash 205084707)
 tile-data (coords vector-2i))

(defgmethod
 (tile-map-layer+is-cell-flipped-h :class 'tile-map-layer :bind
  "is_cell_flipped_h" :hash 3900751641)
 bool (coords vector-2i))

(defgmethod
 (tile-map-layer+is-cell-flipped-v :class 'tile-map-layer :bind
  "is_cell_flipped_v" :hash 3900751641)
 bool (coords vector-2i))

(defgmethod
 (tile-map-layer+is-cell-transposed :class 'tile-map-layer :bind
  "is_cell_transposed" :hash 3900751641)
 bool (coords vector-2i))

(defgmethod
 (tile-map-layer+get-used-cells :class 'tile-map-layer :bind "get_used_cells"
  :hash 3995934104)
 array)

(defgmethod
 (tile-map-layer+get-used-cells-by-id :class 'tile-map-layer :bind
  "get_used_cells_by_id" :hash 4175304538)
 array (source-id int) (atlas-coords vector-2i) (alternative-tile int))

(defgmethod
 (tile-map-layer+get-used-rect :class 'tile-map-layer :bind "get_used_rect"
  :hash 410525958)
 rect-2i)

(defgmethod
 (tile-map-layer+get-pattern :class 'tile-map-layer :bind "get_pattern" :hash
  3820813253)
 tile-map-pattern (coords-array array))

(defgmethod
 (tile-map-layer+set-pattern :class 'tile-map-layer :bind "set_pattern" :hash
  1491151770)
 :void (position vector-2i) (pattern tile-map-pattern))

(defgmethod
 (tile-map-layer+set-cells-terrain-connect :class 'tile-map-layer :bind
  "set_cells_terrain_connect" :hash 748968311)
 :void (cells array) (terrain-set int) (terrain int)
 (ignore-empty-terrains bool))

(defgmethod
 (tile-map-layer+set-cells-terrain-path :class 'tile-map-layer :bind
  "set_cells_terrain_path" :hash 748968311)
 :void (path array) (terrain-set int) (terrain int)
 (ignore-empty-terrains bool))

(defgmethod
 (tile-map-layer+has-body-rid :class 'tile-map-layer :bind "has_body_rid" :hash
  4155700596)
 bool (body rid))

(defgmethod
 (tile-map-layer+get-coords-for-body-rid :class 'tile-map-layer :bind
  "get_coords_for_body_rid" :hash 733700038)
 vector-2i (body rid))

(defgmethod
 (tile-map-layer+update-internals :class 'tile-map-layer :bind
  "update_internals" :hash 3218959716)
 :void)

(defgmethod
 (tile-map-layer+notify-runtime-tile-data-update :class 'tile-map-layer :bind
  "notify_runtime_tile_data_update" :hash 3218959716)
 :void)

(defgmethod
 (tile-map-layer+map-pattern :class 'tile-map-layer :bind "map_pattern" :hash
  1864516957)
 vector-2i (position-in-tilemap vector-2i) (coords-in-pattern vector-2i)
 (pattern tile-map-pattern))

(defgmethod
 (tile-map-layer+get-surrounding-cells :class 'tile-map-layer :bind
  "get_surrounding_cells" :hash 2673526557)
 array (coords vector-2i))

(defgmethod
 (tile-map-layer+get-neighbor-cell :class 'tile-map-layer :bind
  "get_neighbor_cell" :hash 986575103)
 vector-2i (coords vector-2i) (neighbor tile-set+cell-neighbor))

(defgmethod
 (tile-map-layer+map-to-local :class 'tile-map-layer :bind "map_to_local" :hash
  108438297)
 vector-2 (map-position vector-2i))

(defgmethod
 (tile-map-layer+local-to-map :class 'tile-map-layer :bind "local_to_map" :hash
  837806996)
 vector-2i (local-position vector-2))

(defgmethod
 (tile-map-layer+set-tile-map-data-from-array :class 'tile-map-layer :bind
  "set_tile_map_data_from_array" :hash 2971499966)
 :void (tile-map-layer-data packed-byte-array))

(defgmethod
 (tile-map-layer+get-tile-map-data-as-array :class 'tile-map-layer :bind
  "get_tile_map_data_as_array" :hash 2362200018)
 packed-byte-array)

(defgmethod
 (tile-map-layer+set-enabled :class 'tile-map-layer :bind "set_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (tile-map-layer+is-enabled :class 'tile-map-layer :bind "is_enabled" :hash
  36873697)
 bool)

(defgmethod
 (tile-map-layer+set-tile-set :class 'tile-map-layer :bind "set_tile_set" :hash
  774531446)
 :void (tile-set tile-set))

(defgmethod
 (tile-map-layer+get-tile-set :class 'tile-map-layer :bind "get_tile_set" :hash
  2678226422)
 tile-set)

(defgmethod
 (tile-map-layer+set-y-sort-origin :class 'tile-map-layer :bind
  "set_y_sort_origin" :hash 1286410249)
 :void (y-sort-origin int))

(defgmethod
 (tile-map-layer+get-y-sort-origin :class 'tile-map-layer :bind
  "get_y_sort_origin" :hash 3905245786)
 int)

(defgmethod
 (tile-map-layer+set-x-draw-order-reversed :class 'tile-map-layer :bind
  "set_x_draw_order_reversed" :hash 2586408642)
 :void (x-draw-order-reversed bool))

(defgmethod
 (tile-map-layer+is-x-draw-order-reversed :class 'tile-map-layer :bind
  "is_x_draw_order_reversed" :hash 36873697)
 bool)

(defgmethod
 (tile-map-layer+set-rendering-quadrant-size :class 'tile-map-layer :bind
  "set_rendering_quadrant_size" :hash 1286410249)
 :void (size int))

(defgmethod
 (tile-map-layer+get-rendering-quadrant-size :class 'tile-map-layer :bind
  "get_rendering_quadrant_size" :hash 3905245786)
 int)

(defgmethod
 (tile-map-layer+set-collision-enabled :class 'tile-map-layer :bind
  "set_collision_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tile-map-layer+is-collision-enabled :class 'tile-map-layer :bind
  "is_collision_enabled" :hash 36873697)
 bool)

(defgmethod
 (tile-map-layer+set-use-kinematic-bodies :class 'tile-map-layer :bind
  "set_use_kinematic_bodies" :hash 2586408642)
 :void (use-kinematic-bodies bool))

(defgmethod
 (tile-map-layer+is-using-kinematic-bodies :class 'tile-map-layer :bind
  "is_using_kinematic_bodies" :hash 36873697)
 bool)

(defgmethod
 (tile-map-layer+set-collision-visibility-mode :class 'tile-map-layer :bind
  "set_collision_visibility_mode" :hash 3508099847)
 :void (visibility-mode tile-map-layer+debug-visibility-mode))

(defgmethod
 (tile-map-layer+get-collision-visibility-mode :class 'tile-map-layer :bind
  "get_collision_visibility_mode" :hash 338220793)
 tile-map-layer+debug-visibility-mode)

(defgmethod
 (tile-map-layer+set-physics-quadrant-size :class 'tile-map-layer :bind
  "set_physics_quadrant_size" :hash 1286410249)
 :void (size int))

(defgmethod
 (tile-map-layer+get-physics-quadrant-size :class 'tile-map-layer :bind
  "get_physics_quadrant_size" :hash 3905245786)
 int)

(defgmethod
 (tile-map-layer+set-occlusion-enabled :class 'tile-map-layer :bind
  "set_occlusion_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tile-map-layer+is-occlusion-enabled :class 'tile-map-layer :bind
  "is_occlusion_enabled" :hash 36873697)
 bool)

(defgmethod
 (tile-map-layer+set-navigation-enabled :class 'tile-map-layer :bind
  "set_navigation_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tile-map-layer+is-navigation-enabled :class 'tile-map-layer :bind
  "is_navigation_enabled" :hash 36873697)
 bool)

(defgmethod
 (tile-map-layer+set-navigation-map :class 'tile-map-layer :bind
  "set_navigation_map" :hash 2722037293)
 :void (map rid))

(defgmethod
 (tile-map-layer+get-navigation-map :class 'tile-map-layer :bind
  "get_navigation_map" :hash 2944877500)
 rid)

(defgmethod
 (tile-map-layer+set-navigation-visibility-mode :class 'tile-map-layer :bind
  "set_navigation_visibility_mode" :hash 3508099847)
 :void (show-navigation tile-map-layer+debug-visibility-mode))

(defgmethod
 (tile-map-layer+get-navigation-visibility-mode :class 'tile-map-layer :bind
  "get_navigation_visibility_mode" :hash 338220793)
 tile-map-layer+debug-visibility-mode)