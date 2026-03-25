(common-lisp:in-package :%godot)


(defgmethod
 (tile-data+set-flip-h :class 'tile-data :bind "set_flip_h" :hash 2586408642)
 :void (flip-h bool))

(defgmethod
 (tile-data+get-flip-h :class 'tile-data :bind "get_flip_h" :hash 36873697)
 bool)

(defgmethod
 (tile-data+set-flip-v :class 'tile-data :bind "set_flip_v" :hash 2586408642)
 :void (flip-v bool))

(defgmethod
 (tile-data+get-flip-v :class 'tile-data :bind "get_flip_v" :hash 36873697)
 bool)

(defgmethod
 (tile-data+set-transpose :class 'tile-data :bind "set_transpose" :hash
  2586408642)
 :void (transpose bool))

(defgmethod
 (tile-data+get-transpose :class 'tile-data :bind "get_transpose" :hash
  36873697)
 bool)

(defgmethod
 (tile-data+set-material :class 'tile-data :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (tile-data+get-material :class 'tile-data :bind "get_material" :hash 5934680)
 material)

(defgmethod
 (tile-data+set-texture-origin :class 'tile-data :bind "set_texture_origin"
  :hash 1130785943)
 :void (texture-origin vector-2i))

(defgmethod
 (tile-data+get-texture-origin :class 'tile-data :bind "get_texture_origin"
  :hash 3690982128)
 vector-2i)

(defgmethod
 (tile-data+set-modulate :class 'tile-data :bind "set_modulate" :hash
  2920490490)
 :void (modulate color))

(defgmethod
 (tile-data+get-modulate :class 'tile-data :bind "get_modulate" :hash
  3444240500)
 color)

(defgmethod
 (tile-data+set-z-index :class 'tile-data :bind "set_z_index" :hash 1286410249)
 :void (z-index int))

(defgmethod
 (tile-data+get-z-index :class 'tile-data :bind "get_z_index" :hash 3905245786)
 int)

(defgmethod
 (tile-data+set-y-sort-origin :class 'tile-data :bind "set_y_sort_origin" :hash
  1286410249)
 :void (y-sort-origin int))

(defgmethod
 (tile-data+get-y-sort-origin :class 'tile-data :bind "get_y_sort_origin" :hash
  3905245786)
 int)

(defgmethod
 (tile-data+set-occluder-polygons-count :class 'tile-data :bind
  "set_occluder_polygons_count" :hash 3937882851)
 :void (layer-id int) (polygons-count int))

(defgmethod
 (tile-data+get-occluder-polygons-count :class 'tile-data :bind
  "get_occluder_polygons_count" :hash 923996154)
 int (layer-id int))

(defgmethod
 (tile-data+add-occluder-polygon :class 'tile-data :bind "add_occluder_polygon"
  :hash 1286410249)
 :void (layer-id int))

(defgmethod
 (tile-data+remove-occluder-polygon :class 'tile-data :bind
  "remove_occluder_polygon" :hash 3937882851)
 :void (layer-id int) (polygon-index int))

(defgmethod
 (tile-data+set-occluder-polygon :class 'tile-data :bind "set_occluder_polygon"
  :hash 164249167)
 :void (layer-id int) (polygon-index int) (polygon occluder-polygon-2d))

(defgmethod
 (tile-data+get-occluder-polygon :class 'tile-data :bind "get_occluder_polygon"
  :hash 971166743)
 occluder-polygon-2d (layer-id int) (polygon-index int) (flip-h bool)
 (flip-v bool) (transpose bool))

(defgmethod
 (tile-data+set-occluder :class 'tile-data :bind "set_occluder" :hash
  914399637)
 :void (layer-id int) (occluder-polygon occluder-polygon-2d))

(defgmethod
 (tile-data+get-occluder :class 'tile-data :bind "get_occluder" :hash
  2377324099)
 occluder-polygon-2d (layer-id int) (flip-h bool) (flip-v bool)
 (transpose bool))

(defgmethod
 (tile-data+set-constant-linear-velocity :class 'tile-data :bind
  "set_constant_linear_velocity" :hash 163021252)
 :void (layer-id int) (velocity vector-2))

(defgmethod
 (tile-data+get-constant-linear-velocity :class 'tile-data :bind
  "get_constant_linear_velocity" :hash 2299179447)
 vector-2 (layer-id int))

(defgmethod
 (tile-data+set-constant-angular-velocity :class 'tile-data :bind
  "set_constant_angular_velocity" :hash 1602489585)
 :void (layer-id int) (velocity float))

(defgmethod
 (tile-data+get-constant-angular-velocity :class 'tile-data :bind
  "get_constant_angular_velocity" :hash 2339986948)
 float (layer-id int))

(defgmethod
 (tile-data+set-collision-polygons-count :class 'tile-data :bind
  "set_collision_polygons_count" :hash 3937882851)
 :void (layer-id int) (polygons-count int))

(defgmethod
 (tile-data+get-collision-polygons-count :class 'tile-data :bind
  "get_collision_polygons_count" :hash 923996154)
 int (layer-id int))

(defgmethod
 (tile-data+add-collision-polygon :class 'tile-data :bind
  "add_collision_polygon" :hash 1286410249)
 :void (layer-id int))

(defgmethod
 (tile-data+remove-collision-polygon :class 'tile-data :bind
  "remove_collision_polygon" :hash 3937882851)
 :void (layer-id int) (polygon-index int))

(defgmethod
 (tile-data+set-collision-polygon-points :class 'tile-data :bind
  "set_collision_polygon_points" :hash 3230546541)
 :void (layer-id int) (polygon-index int) (polygon packed-vector-2array))

(defgmethod
 (tile-data+get-collision-polygon-points :class 'tile-data :bind
  "get_collision_polygon_points" :hash 103942801)
 packed-vector-2array (layer-id int) (polygon-index int))

(defgmethod
 (tile-data+set-collision-polygon-one-way :class 'tile-data :bind
  "set_collision_polygon_one_way" :hash 1383440665)
 :void (layer-id int) (polygon-index int) (one-way bool))

(defgmethod
 (tile-data+is-collision-polygon-one-way :class 'tile-data :bind
  "is_collision_polygon_one_way" :hash 2522259332)
 bool (layer-id int) (polygon-index int))

(defgmethod
 (tile-data+set-collision-polygon-one-way-margin :class 'tile-data :bind
  "set_collision_polygon_one_way_margin" :hash 3506521499)
 :void (layer-id int) (polygon-index int) (one-way-margin float))

(defgmethod
 (tile-data+get-collision-polygon-one-way-margin :class 'tile-data :bind
  "get_collision_polygon_one_way_margin" :hash 3085491603)
 float (layer-id int) (polygon-index int))

(defgmethod
 (tile-data+set-terrain-set :class 'tile-data :bind "set_terrain_set" :hash
  1286410249)
 :void (terrain-set int))

(defgmethod
 (tile-data+get-terrain-set :class 'tile-data :bind "get_terrain_set" :hash
  3905245786)
 int)

(defgmethod
 (tile-data+set-terrain :class 'tile-data :bind "set_terrain" :hash 1286410249)
 :void (terrain int))

(defgmethod
 (tile-data+get-terrain :class 'tile-data :bind "get_terrain" :hash 3905245786)
 int)

(defgmethod
 (tile-data+set-terrain-peering-bit :class 'tile-data :bind
  "set_terrain_peering_bit" :hash 1084452308)
 :void (peering-bit tile-set+cell-neighbor) (terrain int))

(defgmethod
 (tile-data+get-terrain-peering-bit :class 'tile-data :bind
  "get_terrain_peering_bit" :hash 3831796792)
 int (peering-bit tile-set+cell-neighbor))

(defgmethod
 (tile-data+is-valid-terrain-peering-bit :class 'tile-data :bind
  "is_valid_terrain_peering_bit" :hash 845723972)
 bool (peering-bit tile-set+cell-neighbor))

(defgmethod
 (tile-data+set-navigation-polygon :class 'tile-data :bind
  "set_navigation_polygon" :hash 2224691167)
 :void (layer-id int) (navigation-polygon navigation-polygon))

(defgmethod
 (tile-data+get-navigation-polygon :class 'tile-data :bind
  "get_navigation_polygon" :hash 2907127272)
 navigation-polygon (layer-id int) (flip-h bool) (flip-v bool) (transpose bool))

(defgmethod
 (tile-data+set-probability :class 'tile-data :bind "set_probability" :hash
  373806689)
 :void (probability float))

(defgmethod
 (tile-data+get-probability :class 'tile-data :bind "get_probability" :hash
  1740695150)
 float)

(defgmethod
 (tile-data+set-custom-data :class 'tile-data :bind "set_custom_data" :hash
  402577236)
 :void (layer-name string) (value variant))

(defgmethod
 (tile-data+get-custom-data :class 'tile-data :bind "get_custom_data" :hash
  1868160156)
 variant (layer-name string))

(defgmethod
 (tile-data+has-custom-data :class 'tile-data :bind "has_custom_data" :hash
  3927539163)
 bool (layer-name string))

(defgmethod
 (tile-data+set-custom-data-by-layer-id :class 'tile-data :bind
  "set_custom_data_by_layer_id" :hash 2152698145)
 :void (layer-id int) (value variant))

(defgmethod
 (tile-data+get-custom-data-by-layer-id :class 'tile-data :bind
  "get_custom_data_by_layer_id" :hash 4227898402)
 variant (layer-id int))