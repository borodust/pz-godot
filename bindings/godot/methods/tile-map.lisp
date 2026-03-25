(common-lisp:in-package :%godot)


(defgmethod
 (tile-map+-use-tile-data-runtime-update :class 'tile-map :bind
  "_use_tile_data_runtime_update" :hash 3957903770 :virtual common-lisp:t)
 bool (layer int) (coords vector-2i))

(defgmethod
 (tile-map+-tile-data-runtime-update :class 'tile-map :bind
  "_tile_data_runtime_update" :hash 4223434291 :virtual common-lisp:t)
 :void (layer int) (coords vector-2i) (tile-data tile-data))

(defgmethod
 (tile-map+set-navigation-map :class 'tile-map :bind "set_navigation_map" :hash
  4040184819)
 :void (layer int) (map rid))

(defgmethod
 (tile-map+get-navigation-map :class 'tile-map :bind "get_navigation_map" :hash
  495598643)
 rid (layer int))

(defgmethod
 (tile-map+force-update :class 'tile-map :bind "force_update" :hash 1025054187)
 :void (layer int))

(defgmethod
 (tile-map+set-tileset :class 'tile-map :bind "set_tileset" :hash 774531446)
 :void (tileset tile-set))

(defgmethod
 (tile-map+get-tileset :class 'tile-map :bind "get_tileset" :hash 2678226422)
 tile-set)

(defgmethod
 (tile-map+set-rendering-quadrant-size :class 'tile-map :bind
  "set_rendering_quadrant_size" :hash 1286410249)
 :void (size int))

(defgmethod
 (tile-map+get-rendering-quadrant-size :class 'tile-map :bind
  "get_rendering_quadrant_size" :hash 3905245786)
 int)

(defgmethod
 (tile-map+get-layers-count :class 'tile-map :bind "get_layers_count" :hash
  3905245786)
 int)

(defgmethod
 (tile-map+add-layer :class 'tile-map :bind "add_layer" :hash 1286410249) :void
 (to-position int))

(defgmethod
 (tile-map+move-layer :class 'tile-map :bind "move_layer" :hash 3937882851)
 :void (layer int) (to-position int))

(defgmethod
 (tile-map+remove-layer :class 'tile-map :bind "remove_layer" :hash 1286410249)
 :void (layer int))

(defgmethod
 (tile-map+set-layer-name :class 'tile-map :bind "set_layer_name" :hash
  501894301)
 :void (layer int) (name string))

(defgmethod
 (tile-map+get-layer-name :class 'tile-map :bind "get_layer_name" :hash
  844755477)
 string (layer int))

(defgmethod
 (tile-map+set-layer-enabled :class 'tile-map :bind "set_layer_enabled" :hash
  300928843)
 :void (layer int) (enabled bool))

(defgmethod
 (tile-map+is-layer-enabled :class 'tile-map :bind "is_layer_enabled" :hash
  1116898809)
 bool (layer int))

(defgmethod
 (tile-map+set-layer-modulate :class 'tile-map :bind "set_layer_modulate" :hash
  2878471219)
 :void (layer int) (modulate color))

(defgmethod
 (tile-map+get-layer-modulate :class 'tile-map :bind "get_layer_modulate" :hash
  3457211756)
 color (layer int))

(defgmethod
 (tile-map+set-layer-y-sort-enabled :class 'tile-map :bind
  "set_layer_y_sort_enabled" :hash 300928843)
 :void (layer int) (y-sort-enabled bool))

(defgmethod
 (tile-map+is-layer-y-sort-enabled :class 'tile-map :bind
  "is_layer_y_sort_enabled" :hash 1116898809)
 bool (layer int))

(defgmethod
 (tile-map+set-layer-y-sort-origin :class 'tile-map :bind
  "set_layer_y_sort_origin" :hash 3937882851)
 :void (layer int) (y-sort-origin int))

(defgmethod
 (tile-map+get-layer-y-sort-origin :class 'tile-map :bind
  "get_layer_y_sort_origin" :hash 923996154)
 int (layer int))

(defgmethod
 (tile-map+set-layer-z-index :class 'tile-map :bind "set_layer_z_index" :hash
  3937882851)
 :void (layer int) (z-index int))

(defgmethod
 (tile-map+get-layer-z-index :class 'tile-map :bind "get_layer_z_index" :hash
  923996154)
 int (layer int))

(defgmethod
 (tile-map+set-layer-navigation-enabled :class 'tile-map :bind
  "set_layer_navigation_enabled" :hash 300928843)
 :void (layer int) (enabled bool))

(defgmethod
 (tile-map+is-layer-navigation-enabled :class 'tile-map :bind
  "is_layer_navigation_enabled" :hash 1116898809)
 bool (layer int))

(defgmethod
 (tile-map+set-layer-navigation-map :class 'tile-map :bind
  "set_layer_navigation_map" :hash 4040184819)
 :void (layer int) (map rid))

(defgmethod
 (tile-map+get-layer-navigation-map :class 'tile-map :bind
  "get_layer_navigation_map" :hash 495598643)
 rid (layer int))

(defgmethod
 (tile-map+set-collision-animatable :class 'tile-map :bind
  "set_collision_animatable" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tile-map+is-collision-animatable :class 'tile-map :bind
  "is_collision_animatable" :hash 36873697)
 bool)

(defgmethod
 (tile-map+set-collision-visibility-mode :class 'tile-map :bind
  "set_collision_visibility_mode" :hash 3193440636)
 :void (collision-visibility-mode tile-map+visibility-mode))

(defgmethod
 (tile-map+get-collision-visibility-mode :class 'tile-map :bind
  "get_collision_visibility_mode" :hash 1697018252)
 tile-map+visibility-mode)

(defgmethod
 (tile-map+set-navigation-visibility-mode :class 'tile-map :bind
  "set_navigation_visibility_mode" :hash 3193440636)
 :void (navigation-visibility-mode tile-map+visibility-mode))

(defgmethod
 (tile-map+get-navigation-visibility-mode :class 'tile-map :bind
  "get_navigation_visibility_mode" :hash 1697018252)
 tile-map+visibility-mode)

(defgmethod
 (tile-map+set-cell :class 'tile-map :bind "set_cell" :hash 966713560) :void
 (layer int) (coords vector-2i) (source-id int) (atlas-coords vector-2i)
 (alternative-tile int))

(defgmethod
 (tile-map+erase-cell :class 'tile-map :bind "erase_cell" :hash 2311374912)
 :void (layer int) (coords vector-2i))

(defgmethod
 (tile-map+get-cell-source-id :class 'tile-map :bind "get_cell_source_id" :hash
  551761942)
 int (layer int) (coords vector-2i) (use-proxies bool))

(defgmethod
 (tile-map+get-cell-atlas-coords :class 'tile-map :bind "get_cell_atlas_coords"
  :hash 1869815066)
 vector-2i (layer int) (coords vector-2i) (use-proxies bool))

(defgmethod
 (tile-map+get-cell-alternative-tile :class 'tile-map :bind
  "get_cell_alternative_tile" :hash 551761942)
 int (layer int) (coords vector-2i) (use-proxies bool))

(defgmethod
 (tile-map+get-cell-tile-data :class 'tile-map :bind "get_cell_tile_data" :hash
  2849631287)
 tile-data (layer int) (coords vector-2i) (use-proxies bool))

(defgmethod
 (tile-map+is-cell-flipped-h :class 'tile-map :bind "is_cell_flipped_h" :hash
  2908343862)
 bool (layer int) (coords vector-2i) (use-proxies bool))

(defgmethod
 (tile-map+is-cell-flipped-v :class 'tile-map :bind "is_cell_flipped_v" :hash
  2908343862)
 bool (layer int) (coords vector-2i) (use-proxies bool))

(defgmethod
 (tile-map+is-cell-transposed :class 'tile-map :bind "is_cell_transposed" :hash
  2908343862)
 bool (layer int) (coords vector-2i) (use-proxies bool))

(defgmethod
 (tile-map+get-coords-for-body-rid :class 'tile-map :bind
  "get_coords_for_body_rid" :hash 291584212)
 vector-2i (body rid))

(defgmethod
 (tile-map+get-layer-for-body-rid :class 'tile-map :bind
  "get_layer_for_body_rid" :hash 3917799429)
 int (body rid))

(defgmethod
 (tile-map+get-pattern :class 'tile-map :bind "get_pattern" :hash 2833570986)
 tile-map-pattern (layer int) (coords-array array))

(defgmethod
 (tile-map+map-pattern :class 'tile-map :bind "map_pattern" :hash 1864516957)
 vector-2i (position-in-tilemap vector-2i) (coords-in-pattern vector-2i)
 (pattern tile-map-pattern))

(defgmethod
 (tile-map+set-pattern :class 'tile-map :bind "set_pattern" :hash 1195853946)
 :void (layer int) (position vector-2i) (pattern tile-map-pattern))

(defgmethod
 (tile-map+set-cells-terrain-connect :class 'tile-map :bind
  "set_cells_terrain_connect" :hash 3578627656)
 :void (layer int) (cells array) (terrain-set int) (terrain int)
 (ignore-empty-terrains bool))

(defgmethod
 (tile-map+set-cells-terrain-path :class 'tile-map :bind
  "set_cells_terrain_path" :hash 3578627656)
 :void (layer int) (path array) (terrain-set int) (terrain int)
 (ignore-empty-terrains bool))

(defgmethod
 (tile-map+fix-invalid-tiles :class 'tile-map :bind "fix_invalid_tiles" :hash
  3218959716)
 :void)

(defgmethod
 (tile-map+clear-layer :class 'tile-map :bind "clear_layer" :hash 1286410249)
 :void (layer int))

(defgmethod (tile-map+clear :class 'tile-map :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (tile-map+update-internals :class 'tile-map :bind "update_internals" :hash
  3218959716)
 :void)

(defgmethod
 (tile-map+notify-runtime-tile-data-update :class 'tile-map :bind
  "notify_runtime_tile_data_update" :hash 1025054187)
 :void (layer int))

(defgmethod
 (tile-map+get-surrounding-cells :class 'tile-map :bind "get_surrounding_cells"
  :hash 2673526557)
 array (coords vector-2i))

(defgmethod
 (tile-map+get-used-cells :class 'tile-map :bind "get_used_cells" :hash
  663333327)
 array (layer int))

(defgmethod
 (tile-map+get-used-cells-by-id :class 'tile-map :bind "get_used_cells_by_id"
  :hash 2931012785)
 array (layer int) (source-id int) (atlas-coords vector-2i)
 (alternative-tile int))

(defgmethod
 (tile-map+get-used-rect :class 'tile-map :bind "get_used_rect" :hash
  410525958)
 rect-2i)

(defgmethod
 (tile-map+map-to-local :class 'tile-map :bind "map_to_local" :hash 108438297)
 vector-2 (map-position vector-2i))

(defgmethod
 (tile-map+local-to-map :class 'tile-map :bind "local_to_map" :hash 837806996)
 vector-2i (local-position vector-2))

(defgmethod
 (tile-map+get-neighbor-cell :class 'tile-map :bind "get_neighbor_cell" :hash
  986575103)
 vector-2i (coords vector-2i) (neighbor tile-set+cell-neighbor))