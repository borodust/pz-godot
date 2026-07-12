(common-lisp:in-package :%godot)


(defgmethod
 (tile-set+get-next-source-id :class 'tile-set :bind "get_next_source_id" :hash
  3905245786)
 int)

(defgmethod
 (tile-set+add-source :class 'tile-set :bind "add_source" :hash 1059186179) int
 (source tile-set-source) (atlas-source-id-override int))

(defgmethod
 (tile-set+remove-source :class 'tile-set :bind "remove_source" :hash
  1286410249)
 :void (source-id int))

(defgmethod
 (tile-set+set-source-id :class 'tile-set :bind "set_source_id" :hash
  3937882851)
 :void (source-id int) (new-source-id int))

(defgmethod
 (tile-set+get-source-count :class 'tile-set :bind "get_source_count" :hash
  3905245786)
 int)

(defgmethod
 (tile-set+get-source-id :class 'tile-set :bind "get_source_id" :hash
  923996154)
 int (index int))

(defgmethod
 (tile-set+has-source :class 'tile-set :bind "has_source" :hash 1116898809)
 bool (source-id int))

(defgmethod
 (tile-set+get-source :class 'tile-set :bind "get_source" :hash 1763540252)
 tile-set-source (source-id int))

(defgmethod
 (tile-set+set-tile-shape :class 'tile-set :bind "set_tile_shape" :hash
  2131427112)
 :void (shape tile-set+tile-shape))

(defgmethod
 (tile-set+get-tile-shape :class 'tile-set :bind "get_tile_shape" :hash
  716918169)
 tile-set+tile-shape)

(defgmethod
 (tile-set+set-tile-layout :class 'tile-set :bind "set_tile_layout" :hash
  1071216679)
 :void (layout tile-set+tile-layout))

(defgmethod
 (tile-set+get-tile-layout :class 'tile-set :bind "get_tile_layout" :hash
  194628839)
 tile-set+tile-layout)

(defgmethod
 (tile-set+set-tile-offset-axis :class 'tile-set :bind "set_tile_offset_axis"
  :hash 3300198521)
 :void (alignment tile-set+tile-offset-axis))

(defgmethod
 (tile-set+get-tile-offset-axis :class 'tile-set :bind "get_tile_offset_axis"
  :hash 762494114)
 tile-set+tile-offset-axis)

(defgmethod
 (tile-set+set-tile-size :class 'tile-set :bind "set_tile_size" :hash
  1130785943)
 :void (size vector-2i))

(defgmethod
 (tile-set+get-tile-size :class 'tile-set :bind "get_tile_size" :hash
  3690982128)
 vector-2i)

(defgmethod
 (tile-set+set-uv-clipping :class 'tile-set :bind "set_uv_clipping" :hash
  2586408642)
 :void (uv-clipping bool))

(defgmethod
 (tile-set+is-uv-clipping :class 'tile-set :bind "is_uv_clipping" :hash
  36873697)
 bool)

(defgmethod
 (tile-set+get-occlusion-layers-count :class 'tile-set :bind
  "get_occlusion_layers_count" :hash 3905245786)
 int)

(defgmethod
 (tile-set+add-occlusion-layer :class 'tile-set :bind "add_occlusion_layer"
  :hash 1025054187)
 :void (to-position int))

(defgmethod
 (tile-set+move-occlusion-layer :class 'tile-set :bind "move_occlusion_layer"
  :hash 3937882851)
 :void (layer-index int) (to-position int))

(defgmethod
 (tile-set+remove-occlusion-layer :class 'tile-set :bind
  "remove_occlusion_layer" :hash 1286410249)
 :void (layer-index int))

(defgmethod
 (tile-set+set-occlusion-layer-light-mask :class 'tile-set :bind
  "set_occlusion_layer_light_mask" :hash 3937882851)
 :void (layer-index int) (light-mask int))

(defgmethod
 (tile-set+get-occlusion-layer-light-mask :class 'tile-set :bind
  "get_occlusion_layer_light_mask" :hash 923996154)
 int (layer-index int))

(defgmethod
 (tile-set+set-occlusion-layer-sdf-collision :class 'tile-set :bind
  "set_occlusion_layer_sdf_collision" :hash 300928843)
 :void (layer-index int) (sdf-collision bool))

(defgmethod
 (tile-set+get-occlusion-layer-sdf-collision :class 'tile-set :bind
  "get_occlusion_layer_sdf_collision" :hash 1116898809)
 bool (layer-index int))

(defgmethod
 (tile-set+get-physics-layers-count :class 'tile-set :bind
  "get_physics_layers_count" :hash 3905245786)
 int)

(defgmethod
 (tile-set+add-physics-layer :class 'tile-set :bind "add_physics_layer" :hash
  1025054187)
 :void (to-position int))

(defgmethod
 (tile-set+move-physics-layer :class 'tile-set :bind "move_physics_layer" :hash
  3937882851)
 :void (layer-index int) (to-position int))

(defgmethod
 (tile-set+remove-physics-layer :class 'tile-set :bind "remove_physics_layer"
  :hash 1286410249)
 :void (layer-index int))

(defgmethod
 (tile-set+set-physics-layer-collision-layer :class 'tile-set :bind
  "set_physics_layer_collision_layer" :hash 3937882851)
 :void (layer-index int) (layer int))

(defgmethod
 (tile-set+get-physics-layer-collision-layer :class 'tile-set :bind
  "get_physics_layer_collision_layer" :hash 923996154)
 int (layer-index int))

(defgmethod
 (tile-set+set-physics-layer-collision-mask :class 'tile-set :bind
  "set_physics_layer_collision_mask" :hash 3937882851)
 :void (layer-index int) (mask int))

(defgmethod
 (tile-set+get-physics-layer-collision-mask :class 'tile-set :bind
  "get_physics_layer_collision_mask" :hash 923996154)
 int (layer-index int))

(defgmethod
 (tile-set+set-physics-layer-collision-priority :class 'tile-set :bind
  "set_physics_layer_collision_priority" :hash 1602489585)
 :void (layer-index int) (priority float))

(defgmethod
 (tile-set+get-physics-layer-collision-priority :class 'tile-set :bind
  "get_physics_layer_collision_priority" :hash 2339986948)
 float (layer-index int))

(defgmethod
 (tile-set+set-physics-layer-physics-material :class 'tile-set :bind
  "set_physics_layer_physics_material" :hash 1018687357)
 :void (layer-index int) (physics-material physics-material))

(defgmethod
 (tile-set+get-physics-layer-physics-material :class 'tile-set :bind
  "get_physics_layer_physics_material" :hash 788318639)
 physics-material (layer-index int))

(defgmethod
 (tile-set+get-terrain-sets-count :class 'tile-set :bind
  "get_terrain_sets_count" :hash 3905245786)
 int)

(defgmethod
 (tile-set+add-terrain-set :class 'tile-set :bind "add_terrain_set" :hash
  1025054187)
 :void (to-position int))

(defgmethod
 (tile-set+move-terrain-set :class 'tile-set :bind "move_terrain_set" :hash
  3937882851)
 :void (terrain-set int) (to-position int))

(defgmethod
 (tile-set+remove-terrain-set :class 'tile-set :bind "remove_terrain_set" :hash
  1286410249)
 :void (terrain-set int))

(defgmethod
 (tile-set+set-terrain-set-mode :class 'tile-set :bind "set_terrain_set_mode"
  :hash 3943003916)
 :void (terrain-set int) (mode tile-set+terrain-mode))

(defgmethod
 (tile-set+get-terrain-set-mode :class 'tile-set :bind "get_terrain_set_mode"
  :hash 2084469411)
 tile-set+terrain-mode (terrain-set int))

(defgmethod
 (tile-set+get-terrains-count :class 'tile-set :bind "get_terrains_count" :hash
  923996154)
 int (terrain-set int))

(defgmethod
 (tile-set+add-terrain :class 'tile-set :bind "add_terrain" :hash 1230568737)
 :void (terrain-set int) (to-position int))

(defgmethod
 (tile-set+move-terrain :class 'tile-set :bind "move_terrain" :hash 1649997291)
 :void (terrain-set int) (terrain-index int) (to-position int))

(defgmethod
 (tile-set+remove-terrain :class 'tile-set :bind "remove_terrain" :hash
  3937882851)
 :void (terrain-set int) (terrain-index int))

(defgmethod
 (tile-set+clear-terrains :class 'tile-set :bind "clear_terrains" :hash
  1286410249)
 :void (terrain-set int))

(defgmethod
 (tile-set+set-terrain-name :class 'tile-set :bind "set_terrain_name" :hash
  2285447957)
 :void (terrain-set int) (terrain-index int) (name string))

(defgmethod
 (tile-set+get-terrain-name :class 'tile-set :bind "get_terrain_name" :hash
  1391810591)
 string (terrain-set int) (terrain-index int))

(defgmethod
 (tile-set+set-terrain-color :class 'tile-set :bind "set_terrain_color" :hash
  3733378741)
 :void (terrain-set int) (terrain-index int) (color color))

(defgmethod
 (tile-set+get-terrain-color :class 'tile-set :bind "get_terrain_color" :hash
  2165839948)
 color (terrain-set int) (terrain-index int))

(defgmethod
 (tile-set+get-navigation-layers-count :class 'tile-set :bind
  "get_navigation_layers_count" :hash 3905245786)
 int)

(defgmethod
 (tile-set+add-navigation-layer :class 'tile-set :bind "add_navigation_layer"
  :hash 1025054187)
 :void (to-position int))

(defgmethod
 (tile-set+move-navigation-layer :class 'tile-set :bind "move_navigation_layer"
  :hash 3937882851)
 :void (layer-index int) (to-position int))

(defgmethod
 (tile-set+remove-navigation-layer :class 'tile-set :bind
  "remove_navigation_layer" :hash 1286410249)
 :void (layer-index int))

(defgmethod
 (tile-set+set-navigation-layer-layers :class 'tile-set :bind
  "set_navigation_layer_layers" :hash 3937882851)
 :void (layer-index int) (layers int))

(defgmethod
 (tile-set+get-navigation-layer-layers :class 'tile-set :bind
  "get_navigation_layer_layers" :hash 923996154)
 int (layer-index int))

(defgmethod
 (tile-set+set-navigation-layer-layer-value :class 'tile-set :bind
  "set_navigation_layer_layer_value" :hash 1383440665)
 :void (layer-index int) (layer-number int) (value bool))

(defgmethod
 (tile-set+get-navigation-layer-layer-value :class 'tile-set :bind
  "get_navigation_layer_layer_value" :hash 2522259332)
 bool (layer-index int) (layer-number int))

(defgmethod
 (tile-set+get-custom-data-layers-count :class 'tile-set :bind
  "get_custom_data_layers_count" :hash 3905245786)
 int)

(defgmethod
 (tile-set+add-custom-data-layer :class 'tile-set :bind "add_custom_data_layer"
  :hash 1025054187)
 :void (to-position int))

(defgmethod
 (tile-set+move-custom-data-layer :class 'tile-set :bind
  "move_custom_data_layer" :hash 3937882851)
 :void (layer-index int) (to-position int))

(defgmethod
 (tile-set+remove-custom-data-layer :class 'tile-set :bind
  "remove_custom_data_layer" :hash 1286410249)
 :void (layer-index int))

(defgmethod
 (tile-set+get-custom-data-layer-by-name :class 'tile-set :bind
  "get_custom_data_layer_by_name" :hash 1321353865)
 int (layer-name string))

(defgmethod
 (tile-set+set-custom-data-layer-name :class 'tile-set :bind
  "set_custom_data_layer_name" :hash 501894301)
 :void (layer-index int) (layer-name string))

(defgmethod
 (tile-set+has-custom-data-layer-by-name :class 'tile-set :bind
  "has_custom_data_layer_by_name" :hash 3927539163)
 bool (layer-name string))

(defgmethod
 (tile-set+get-custom-data-layer-name :class 'tile-set :bind
  "get_custom_data_layer_name" :hash 844755477)
 string (layer-index int))

(defgmethod
 (tile-set+set-custom-data-layer-type :class 'tile-set :bind
  "set_custom_data_layer_type" :hash 3492912874)
 :void (layer-index int) (layer-type variant+type))

(defgmethod
 (tile-set+get-custom-data-layer-type :class 'tile-set :bind
  "get_custom_data_layer_type" :hash 2990820875)
 variant+type (layer-index int))

(defgmethod
 (tile-set+set-source-level-tile-proxy :class 'tile-set :bind
  "set_source_level_tile_proxy" :hash 3937882851)
 :void (source-from int) (source-to int))

(defgmethod
 (tile-set+get-source-level-tile-proxy :class 'tile-set :bind
  "get_source_level_tile_proxy" :hash 3744713108)
 int (source-from int))

(defgmethod
 (tile-set+has-source-level-tile-proxy :class 'tile-set :bind
  "has_source_level_tile_proxy" :hash 3067735520)
 bool (source-from int))

(defgmethod
 (tile-set+remove-source-level-tile-proxy :class 'tile-set :bind
  "remove_source_level_tile_proxy" :hash 1286410249)
 :void (source-from int))

(defgmethod
 (tile-set+set-coords-level-tile-proxy :class 'tile-set :bind
  "set_coords_level_tile_proxy" :hash 1769939278)
 :void (source-from int) (coords-from vector-2i) (source-to int)
 (coords-to vector-2i))

(defgmethod
 (tile-set+get-coords-level-tile-proxy :class 'tile-set :bind
  "get_coords_level_tile_proxy" :hash 2856536371)
 array (source-from int) (coords-from vector-2i))

(defgmethod
 (tile-set+has-coords-level-tile-proxy :class 'tile-set :bind
  "has_coords_level_tile_proxy" :hash 3957903770)
 bool (source-from int) (coords-from vector-2i))

(defgmethod
 (tile-set+remove-coords-level-tile-proxy :class 'tile-set :bind
  "remove_coords_level_tile_proxy" :hash 2311374912)
 :void (source-from int) (coords-from vector-2i))

(defgmethod
 (tile-set+set-alternative-level-tile-proxy :class 'tile-set :bind
  "set_alternative_level_tile_proxy" :hash 3862385460)
 :void (source-from int) (coords-from vector-2i) (alternative-from int)
 (source-to int) (coords-to vector-2i) (alternative-to int))

(defgmethod
 (tile-set+get-alternative-level-tile-proxy :class 'tile-set :bind
  "get_alternative_level_tile_proxy" :hash 2303761075)
 array (source-from int) (coords-from vector-2i) (alternative-from int))

(defgmethod
 (tile-set+has-alternative-level-tile-proxy :class 'tile-set :bind
  "has_alternative_level_tile_proxy" :hash 180086755)
 bool (source-from int) (coords-from vector-2i) (alternative-from int))

(defgmethod
 (tile-set+remove-alternative-level-tile-proxy :class 'tile-set :bind
  "remove_alternative_level_tile_proxy" :hash 2328951467)
 :void (source-from int) (coords-from vector-2i) (alternative-from int))

(defgmethod
 (tile-set+map-tile-proxy :class 'tile-set :bind "map_tile_proxy" :hash
  4267935328)
 array (source-from int) (coords-from vector-2i) (alternative-from int))

(defgmethod
 (tile-set+cleanup-invalid-tile-proxies :class 'tile-set :bind
  "cleanup_invalid_tile_proxies" :hash 3218959716)
 :void)

(defgmethod
 (tile-set+clear-tile-proxies :class 'tile-set :bind "clear_tile_proxies" :hash
  3218959716)
 :void)

(defgmethod
 (tile-set+add-pattern :class 'tile-set :bind "add_pattern" :hash 763712015)
 int (pattern tile-map-pattern) (index int))

(defgmethod
 (tile-set+get-pattern :class 'tile-set :bind "get_pattern" :hash 4207737510)
 tile-map-pattern (index int))

(defgmethod
 (tile-set+remove-pattern :class 'tile-set :bind "remove_pattern" :hash
  1286410249)
 :void (index int))

(defgmethod
 (tile-set+get-patterns-count :class 'tile-set :bind "get_patterns_count" :hash
  2455072627)
 int)