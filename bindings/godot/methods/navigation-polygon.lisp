(common-lisp:in-package :%godot)


(defgmethod
 (navigation-polygon+set-vertices :class 'navigation-polygon :bind
  "set_vertices" :hash 1509147220)
 :void (vertices packed-vector-2array))

(defgmethod
 (navigation-polygon+get-vertices :class 'navigation-polygon :bind
  "get_vertices" :hash 2961356807)
 packed-vector-2array)

(defgmethod
 (navigation-polygon+add-polygon :class 'navigation-polygon :bind "add_polygon"
  :hash 3614634198)
 :void (polygon packed-int-32array))

(defgmethod
 (navigation-polygon+get-polygon-count :class 'navigation-polygon :bind
  "get_polygon_count" :hash 3905245786)
 int)

(defgmethod
 (navigation-polygon+get-polygon :class 'navigation-polygon :bind "get_polygon"
  :hash 3668444399)
 packed-int-32array (idx int))

(defgmethod
 (navigation-polygon+clear-polygons :class 'navigation-polygon :bind
  "clear_polygons" :hash 3218959716)
 :void)

(defgmethod
 (navigation-polygon+get-navigation-mesh :class 'navigation-polygon :bind
  "get_navigation_mesh" :hash 330232164)
 navigation-mesh)

(defgmethod
 (navigation-polygon+add-outline :class 'navigation-polygon :bind "add_outline"
  :hash 1509147220)
 :void (outline packed-vector-2array))

(defgmethod
 (navigation-polygon+add-outline-at-index :class 'navigation-polygon :bind
  "add_outline_at_index" :hash 1569738947)
 :void (outline packed-vector-2array) (index int))

(defgmethod
 (navigation-polygon+get-outline-count :class 'navigation-polygon :bind
  "get_outline_count" :hash 3905245786)
 int)

(defgmethod
 (navigation-polygon+set-outline :class 'navigation-polygon :bind "set_outline"
  :hash 1201971903)
 :void (idx int) (outline packed-vector-2array))

(defgmethod
 (navigation-polygon+get-outline :class 'navigation-polygon :bind "get_outline"
  :hash 3946907486)
 packed-vector-2array (idx int))

(defgmethod
 (navigation-polygon+remove-outline :class 'navigation-polygon :bind
  "remove_outline" :hash 1286410249)
 :void (idx int))

(defgmethod
 (navigation-polygon+clear-outlines :class 'navigation-polygon :bind
  "clear_outlines" :hash 3218959716)
 :void)

(defgmethod
 (navigation-polygon+make-polygons-from-outlines :class 'navigation-polygon
  :bind "make_polygons_from_outlines" :hash 3218959716)
 :void)

(defgmethod
 (navigation-polygon+set-cell-size :class 'navigation-polygon :bind
  "set_cell_size" :hash 373806689)
 :void (cell-size float))

(defgmethod
 (navigation-polygon+get-cell-size :class 'navigation-polygon :bind
  "get_cell_size" :hash 1740695150)
 float)

(defgmethod
 (navigation-polygon+set-border-size :class 'navigation-polygon :bind
  "set_border_size" :hash 373806689)
 :void (border-size float))

(defgmethod
 (navigation-polygon+get-border-size :class 'navigation-polygon :bind
  "get_border_size" :hash 1740695150)
 float)

(defgmethod
 (navigation-polygon+set-sample-partition-type :class 'navigation-polygon :bind
  "set_sample_partition_type" :hash 2441478482)
 :void (sample-partition-type navigation-polygon+sample-partition-type))

(defgmethod
 (navigation-polygon+get-sample-partition-type :class 'navigation-polygon :bind
  "get_sample_partition_type" :hash 3887422851)
 navigation-polygon+sample-partition-type)

(defgmethod
 (navigation-polygon+set-parsed-geometry-type :class 'navigation-polygon :bind
  "set_parsed_geometry_type" :hash 2507971764)
 :void (geometry-type navigation-polygon+parsed-geometry-type))

(defgmethod
 (navigation-polygon+get-parsed-geometry-type :class 'navigation-polygon :bind
  "get_parsed_geometry_type" :hash 1073219508)
 navigation-polygon+parsed-geometry-type)

(defgmethod
 (navigation-polygon+set-parsed-collision-mask :class 'navigation-polygon :bind
  "set_parsed_collision_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (navigation-polygon+get-parsed-collision-mask :class 'navigation-polygon :bind
  "get_parsed_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (navigation-polygon+set-parsed-collision-mask-value :class 'navigation-polygon
  :bind "set_parsed_collision_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (navigation-polygon+get-parsed-collision-mask-value :class 'navigation-polygon
  :bind "get_parsed_collision_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (navigation-polygon+set-source-geometry-mode :class 'navigation-polygon :bind
  "set_source_geometry_mode" :hash 4002316705)
 :void (geometry-mode navigation-polygon+source-geometry-mode))

(defgmethod
 (navigation-polygon+get-source-geometry-mode :class 'navigation-polygon :bind
  "get_source_geometry_mode" :hash 459686762)
 navigation-polygon+source-geometry-mode)

(defgmethod
 (navigation-polygon+set-source-geometry-group-name :class 'navigation-polygon
  :bind "set_source_geometry_group_name" :hash 3304788590)
 :void (group-name string-name))

(defgmethod
 (navigation-polygon+get-source-geometry-group-name :class 'navigation-polygon
  :bind "get_source_geometry_group_name" :hash 2002593661)
 string-name)

(defgmethod
 (navigation-polygon+set-agent-radius :class 'navigation-polygon :bind
  "set_agent_radius" :hash 373806689)
 :void (agent-radius float))

(defgmethod
 (navigation-polygon+get-agent-radius :class 'navigation-polygon :bind
  "get_agent_radius" :hash 1740695150)
 float)

(defgmethod
 (navigation-polygon+set-baking-rect :class 'navigation-polygon :bind
  "set_baking_rect" :hash 2046264180)
 :void (rect rect-2))

(defgmethod
 (navigation-polygon+get-baking-rect :class 'navigation-polygon :bind
  "get_baking_rect" :hash 1639390495)
 rect-2)

(defgmethod
 (navigation-polygon+set-baking-rect-offset :class 'navigation-polygon :bind
  "set_baking_rect_offset" :hash 743155724)
 :void (rect-offset vector-2))

(defgmethod
 (navigation-polygon+get-baking-rect-offset :class 'navigation-polygon :bind
  "get_baking_rect_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (navigation-polygon+clear :class 'navigation-polygon :bind "clear" :hash
  3218959716)
 :void)