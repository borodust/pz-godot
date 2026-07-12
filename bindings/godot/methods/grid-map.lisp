(common-lisp:in-package :%godot)


(defgmethod
 (grid-map+set-collision-layer :class 'grid-map :bind "set_collision_layer"
  :hash 1286410249)
 :void (layer int))

(defgmethod
 (grid-map+get-collision-layer :class 'grid-map :bind "get_collision_layer"
  :hash 3905245786)
 int)

(defgmethod
 (grid-map+set-collision-mask :class 'grid-map :bind "set_collision_mask" :hash
  1286410249)
 :void (mask int))

(defgmethod
 (grid-map+get-collision-mask :class 'grid-map :bind "get_collision_mask" :hash
  3905245786)
 int)

(defgmethod
 (grid-map+set-collision-mask-value :class 'grid-map :bind
  "set_collision_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (grid-map+get-collision-mask-value :class 'grid-map :bind
  "get_collision_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (grid-map+set-collision-layer-value :class 'grid-map :bind
  "set_collision_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (grid-map+get-collision-layer-value :class 'grid-map :bind
  "get_collision_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (grid-map+set-collision-priority :class 'grid-map :bind
  "set_collision_priority" :hash 373806689)
 :void (priority float))

(defgmethod
 (grid-map+get-collision-priority :class 'grid-map :bind
  "get_collision_priority" :hash 1740695150)
 float)

(defgmethod
 (grid-map+set-collision-visibility-mode :class 'grid-map :bind
  "set_collision_visibility_mode" :hash 4160694578)
 :void (visibility-mode grid-map+debug-visibility-mode))

(defgmethod
 (grid-map+get-collision-visibility-mode :class 'grid-map :bind
  "get_collision_visibility_mode" :hash 3729798365)
 grid-map+debug-visibility-mode)

(defgmethod
 (grid-map+set-physics-material :class 'grid-map :bind "set_physics_material"
  :hash 1784508650)
 :void (material physics-material))

(defgmethod
 (grid-map+get-physics-material :class 'grid-map :bind "get_physics_material"
  :hash 2521850424)
 physics-material)

(defgmethod
 (grid-map+set-bake-navigation :class 'grid-map :bind "set_bake_navigation"
  :hash 2586408642)
 :void (bake-navigation bool))

(defgmethod
 (grid-map+is-baking-navigation :class 'grid-map :bind "is_baking_navigation"
  :hash 2240911060)
 bool)

(defgmethod
 (grid-map+set-navigation-map :class 'grid-map :bind "set_navigation_map" :hash
  2722037293)
 :void (navigation-map rid))

(defgmethod
 (grid-map+get-navigation-map :class 'grid-map :bind "get_navigation_map" :hash
  2944877500)
 rid)

(defgmethod
 (grid-map+set-mesh-library :class 'grid-map :bind "set_mesh_library" :hash
  1488083439)
 :void (mesh-library mesh-library))

(defgmethod
 (grid-map+get-mesh-library :class 'grid-map :bind "get_mesh_library" :hash
  3350993772)
 mesh-library)

(defgmethod
 (grid-map+set-cell-size :class 'grid-map :bind "set_cell_size" :hash
  3460891852)
 :void (size vector-3))

(defgmethod
 (grid-map+get-cell-size :class 'grid-map :bind "get_cell_size" :hash
  3360562783)
 vector-3)

(defgmethod
 (grid-map+set-cell-scale :class 'grid-map :bind "set_cell_scale" :hash
  373806689)
 :void (scale float))

(defgmethod
 (grid-map+get-cell-scale :class 'grid-map :bind "get_cell_scale" :hash
  1740695150)
 float)

(defgmethod
 (grid-map+set-octant-size :class 'grid-map :bind "set_octant_size" :hash
  1286410249)
 :void (size int))

(defgmethod
 (grid-map+get-octant-size :class 'grid-map :bind "get_octant_size" :hash
  3905245786)
 int)

(defgmethod
 (grid-map+set-cell-item :class 'grid-map :bind "set_cell_item" :hash
  3449088946)
 :void (position vector-3i) (item int) (orientation int))

(defgmethod
 (grid-map+get-cell-item :class 'grid-map :bind "get_cell_item" :hash
  3724960147)
 int (position vector-3i))

(defgmethod
 (grid-map+get-cell-item-orientation :class 'grid-map :bind
  "get_cell_item_orientation" :hash 3724960147)
 int (position vector-3i))

(defgmethod
 (grid-map+get-cell-item-basis :class 'grid-map :bind "get_cell_item_basis"
  :hash 3493604918)
 basis (position vector-3i))

(defgmethod
 (grid-map+get-basis-with-orthogonal-index :class 'grid-map :bind
  "get_basis_with_orthogonal_index" :hash 2816196998)
 basis (index int))

(defgmethod
 (grid-map+get-orthogonal-index-from-basis :class 'grid-map :bind
  "get_orthogonal_index_from_basis" :hash 4210359952)
 int (basis basis))

(defgmethod
 (grid-map+local-to-map :class 'grid-map :bind "local_to_map" :hash 1257687843)
 vector-3i (local-position vector-3))

(defgmethod
 (grid-map+map-to-local :class 'grid-map :bind "map_to_local" :hash 1088329196)
 vector-3 (map-position vector-3i))

(defgmethod
 (grid-map+resource-changed :class 'grid-map :bind "resource_changed" :hash
  968641751)
 :void (resource resource))

(defgmethod
 (grid-map+set-center-x :class 'grid-map :bind "set_center_x" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (grid-map+get-center-x :class 'grid-map :bind "get_center_x" :hash 36873697)
 bool)

(defgmethod
 (grid-map+set-center-y :class 'grid-map :bind "set_center_y" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (grid-map+get-center-y :class 'grid-map :bind "get_center_y" :hash 36873697)
 bool)

(defgmethod
 (grid-map+set-center-z :class 'grid-map :bind "set_center_z" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (grid-map+get-center-z :class 'grid-map :bind "get_center_z" :hash 36873697)
 bool)

(defgmethod (grid-map+clear :class 'grid-map :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (grid-map+get-used-cells :class 'grid-map :bind "get_used_cells" :hash
  3995934104)
 array)

(defgmethod
 (grid-map+get-used-cells-by-item :class 'grid-map :bind
  "get_used_cells_by_item" :hash 663333327)
 array (item int))

(defgmethod
 (grid-map+get-used-octants :class 'grid-map :bind "get_used_octants" :hash
  3995934104)
 array)

(defgmethod
 (grid-map+get-used-octants-by-item :class 'grid-map :bind
  "get_used_octants_by_item" :hash 663333327)
 array (item int))

(defgmethod
 (grid-map+get-used-cells-in-octant :class 'grid-map :bind
  "get_used_cells_in_octant" :hash 2658725580)
 array (octant-coords vector-3i))

(defgmethod
 (grid-map+get-used-cells-in-octant-by-item :class 'grid-map :bind
  "get_used_cells_in_octant_by_item" :hash 2384667821)
 array (octant-coords vector-3i) (item int))

(defgmethod
 (grid-map+get-octants-in-bounds :class 'grid-map :bind "get_octants_in_bounds"
  :hash 2489849902)
 array (bounds aabb))

(defgmethod
 (grid-map+get-used-octants-in-bounds :class 'grid-map :bind
  "get_used_octants_in_bounds" :hash 2489849902)
 array (bounds aabb))

(defgmethod
 (grid-map+get-octant-coords-from-cell-coords :class 'grid-map :bind
  "get_octant_coords_from_cell_coords" :hash 2075501597)
 vector-3i (cell-coords vector-3i))

(defgmethod
 (grid-map+get-meshes :class 'grid-map :bind "get_meshes" :hash 3995934104)
 array)

(defgmethod
 (grid-map+get-bake-meshes :class 'grid-map :bind "get_bake_meshes" :hash
  2915620761)
 array)

(defgmethod
 (grid-map+get-bake-mesh-instance :class 'grid-map :bind
  "get_bake_mesh_instance" :hash 937000113)
 rid (idx int))

(defgmethod
 (grid-map+clear-baked-meshes :class 'grid-map :bind "clear_baked_meshes" :hash
  3218959716)
 :void)

(defgmethod
 (grid-map+make-baked-meshes :class 'grid-map :bind "make_baked_meshes" :hash
  3609286057)
 :void (gen-lightmap-uv bool) (lightmap-uv-texel-size float))