(common-lisp:in-package :%godot)


(defgmethod
 (graph-edit+%is-in-input-hotzone :class 'graph-edit :bind
  "_is_in_input_hotzone" :hash 1779768129 :virtual common-lisp:t)
 bool (in-node object) (in-port int) (mouse-position vector-2))

(defgmethod
 (graph-edit+%is-in-output-hotzone :class 'graph-edit :bind
  "_is_in_output_hotzone" :hash 1779768129 :virtual common-lisp:t)
 bool (in-node object) (in-port int) (mouse-position vector-2))

(defgmethod
 (graph-edit+%get-connection-line :class 'graph-edit :bind
  "_get_connection_line" :hash 3932192302 :virtual common-lisp:t)
 packed-vector-2array (from-position vector-2) (to-position vector-2))

(defgmethod
 (graph-edit+%is-node-hover-valid :class 'graph-edit :bind
  "_is_node_hover_valid" :hash 4216241294 :virtual common-lisp:t)
 bool (from-node string-name) (from-port int) (to-node string-name)
 (to-port int))

(defgmethod
 (graph-edit+connect-node :class 'graph-edit :bind "connect_node" :hash
  1376144231)
 error (from-node string-name) (from-port int) (to-node string-name)
 (to-port int) (keep-alive bool))

(defgmethod
 (graph-edit+is-node-connected :class 'graph-edit :bind "is_node_connected"
  :hash 4216241294)
 bool (from-node string-name) (from-port int) (to-node string-name)
 (to-port int))

(defgmethod
 (graph-edit+disconnect-node :class 'graph-edit :bind "disconnect_node" :hash
  1933654315)
 :void (from-node string-name) (from-port int) (to-node string-name)
 (to-port int))

(defgmethod
 (graph-edit+set-connection-activity :class 'graph-edit :bind
  "set_connection_activity" :hash 1141899943)
 :void (from-node string-name) (from-port int) (to-node string-name)
 (to-port int) (amount float))

(defgmethod
 (graph-edit+set-connections :class 'graph-edit :bind "set_connections" :hash
  381264803)
 :void (connections array))

(defgmethod
 (graph-edit+get-connection-list :class 'graph-edit :bind "get_connection_list"
  :hash 3995934104)
 array)

(defgmethod
 (graph-edit+get-connection-count :class 'graph-edit :bind
  "get_connection_count" :hash 861718734)
 int (from-node string-name) (from-port int))

(defgmethod
 (graph-edit+get-closest-connection-at-point :class 'graph-edit :bind
  "get_closest_connection_at_point" :hash 453879819)
 dictionary (point vector-2) (max-distance float))

(defgmethod
 (graph-edit+get-connection-list-from-node :class 'graph-edit :bind
  "get_connection_list_from_node" :hash 3147814860)
 array (node string-name))

(defgmethod
 (graph-edit+get-connections-intersecting-with-rect :class 'graph-edit :bind
  "get_connections_intersecting_with_rect" :hash 2709748719)
 array (rect rect-2))

(defgmethod
 (graph-edit+clear-connections :class 'graph-edit :bind "clear_connections"
  :hash 3218959716)
 :void)

(defgmethod
 (graph-edit+force-connection-drag-end :class 'graph-edit :bind
  "force_connection_drag_end" :hash 3218959716)
 :void)

(defgmethod
 (graph-edit+get-scroll-offset :class 'graph-edit :bind "get_scroll_offset"
  :hash 3341600327)
 vector-2)

(defgmethod
 (graph-edit+set-scroll-offset :class 'graph-edit :bind "set_scroll_offset"
  :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (graph-edit+add-valid-right-disconnect-type :class 'graph-edit :bind
  "add_valid_right_disconnect_type" :hash 1286410249)
 :void (type int))

(defgmethod
 (graph-edit+remove-valid-right-disconnect-type :class 'graph-edit :bind
  "remove_valid_right_disconnect_type" :hash 1286410249)
 :void (type int))

(defgmethod
 (graph-edit+add-valid-left-disconnect-type :class 'graph-edit :bind
  "add_valid_left_disconnect_type" :hash 1286410249)
 :void (type int))

(defgmethod
 (graph-edit+remove-valid-left-disconnect-type :class 'graph-edit :bind
  "remove_valid_left_disconnect_type" :hash 1286410249)
 :void (type int))

(defgmethod
 (graph-edit+add-valid-connection-type :class 'graph-edit :bind
  "add_valid_connection_type" :hash 3937882851)
 :void (from-type int) (to-type int))

(defgmethod
 (graph-edit+remove-valid-connection-type :class 'graph-edit :bind
  "remove_valid_connection_type" :hash 3937882851)
 :void (from-type int) (to-type int))

(defgmethod
 (graph-edit+is-valid-connection-type :class 'graph-edit :bind
  "is_valid_connection_type" :hash 2522259332)
 bool (from-type int) (to-type int))

(defgmethod
 (graph-edit+get-connection-line :class 'graph-edit :bind "get_connection_line"
  :hash 3932192302)
 packed-vector-2array (from-node vector-2) (to-node vector-2))

(defgmethod
 (graph-edit+attach-graph-element-to-frame :class 'graph-edit :bind
  "attach_graph_element_to_frame" :hash 3740211285)
 :void (element string-name) (frame string-name))

(defgmethod
 (graph-edit+detach-graph-element-from-frame :class 'graph-edit :bind
  "detach_graph_element_from_frame" :hash 3304788590)
 :void (element string-name))

(defgmethod
 (graph-edit+get-element-frame :class 'graph-edit :bind "get_element_frame"
  :hash 988084372)
 graph-frame (element string-name))

(defgmethod
 (graph-edit+get-attached-nodes-of-frame :class 'graph-edit :bind
  "get_attached_nodes_of_frame" :hash 689397652)
 array (frame string-name))

(defgmethod
 (graph-edit+set-panning-scheme :class 'graph-edit :bind "set_panning_scheme"
  :hash 18893313)
 :void (scheme graph-edit+panning-scheme))

(defgmethod
 (graph-edit+get-panning-scheme :class 'graph-edit :bind "get_panning_scheme"
  :hash 549924446)
 graph-edit+panning-scheme)

(defgmethod
 (graph-edit+set-zoom :class 'graph-edit :bind "set_zoom" :hash 373806689)
 :void (zoom float))

(defgmethod
 (graph-edit+get-zoom :class 'graph-edit :bind "get_zoom" :hash 1740695150)
 float)

(defgmethod
 (graph-edit+set-zoom-min :class 'graph-edit :bind "set_zoom_min" :hash
  373806689)
 :void (zoom-min float))

(defgmethod
 (graph-edit+get-zoom-min :class 'graph-edit :bind "get_zoom_min" :hash
  1740695150)
 float)

(defgmethod
 (graph-edit+set-zoom-max :class 'graph-edit :bind "set_zoom_max" :hash
  373806689)
 :void (zoom-max float))

(defgmethod
 (graph-edit+get-zoom-max :class 'graph-edit :bind "get_zoom_max" :hash
  1740695150)
 float)

(defgmethod
 (graph-edit+set-zoom-step :class 'graph-edit :bind "set_zoom_step" :hash
  373806689)
 :void (zoom-step float))

(defgmethod
 (graph-edit+get-zoom-step :class 'graph-edit :bind "get_zoom_step" :hash
  1740695150)
 float)

(defgmethod
 (graph-edit+set-show-grid :class 'graph-edit :bind "set_show_grid" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (graph-edit+is-showing-grid :class 'graph-edit :bind "is_showing_grid" :hash
  36873697)
 bool)

(defgmethod
 (graph-edit+set-grid-pattern :class 'graph-edit :bind "set_grid_pattern" :hash
  1074098205)
 :void (pattern graph-edit+grid-pattern))

(defgmethod
 (graph-edit+get-grid-pattern :class 'graph-edit :bind "get_grid_pattern" :hash
  1286127528)
 graph-edit+grid-pattern)

(defgmethod
 (graph-edit+set-snapping-enabled :class 'graph-edit :bind
  "set_snapping_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (graph-edit+is-snapping-enabled :class 'graph-edit :bind "is_snapping_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (graph-edit+set-snapping-distance :class 'graph-edit :bind
  "set_snapping_distance" :hash 1286410249)
 :void (pixels int))

(defgmethod
 (graph-edit+get-snapping-distance :class 'graph-edit :bind
  "get_snapping_distance" :hash 3905245786)
 int)

(defgmethod
 (graph-edit+set-connection-lines-curvature :class 'graph-edit :bind
  "set_connection_lines_curvature" :hash 373806689)
 :void (curvature float))

(defgmethod
 (graph-edit+get-connection-lines-curvature :class 'graph-edit :bind
  "get_connection_lines_curvature" :hash 1740695150)
 float)

(defgmethod
 (graph-edit+set-connection-lines-thickness :class 'graph-edit :bind
  "set_connection_lines_thickness" :hash 373806689)
 :void (pixels float))

(defgmethod
 (graph-edit+get-connection-lines-thickness :class 'graph-edit :bind
  "get_connection_lines_thickness" :hash 1740695150)
 float)

(defgmethod
 (graph-edit+set-connection-lines-antialiased :class 'graph-edit :bind
  "set_connection_lines_antialiased" :hash 2586408642)
 :void (pixels bool))

(defgmethod
 (graph-edit+is-connection-lines-antialiased :class 'graph-edit :bind
  "is_connection_lines_antialiased" :hash 36873697)
 bool)

(defgmethod
 (graph-edit+set-minimap-size :class 'graph-edit :bind "set_minimap_size" :hash
  743155724)
 :void (size vector-2))

(defgmethod
 (graph-edit+get-minimap-size :class 'graph-edit :bind "get_minimap_size" :hash
  3341600327)
 vector-2)

(defgmethod
 (graph-edit+set-minimap-opacity :class 'graph-edit :bind "set_minimap_opacity"
  :hash 373806689)
 :void (opacity float))

(defgmethod
 (graph-edit+get-minimap-opacity :class 'graph-edit :bind "get_minimap_opacity"
  :hash 1740695150)
 float)

(defgmethod
 (graph-edit+set-minimap-enabled :class 'graph-edit :bind "set_minimap_enabled"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (graph-edit+is-minimap-enabled :class 'graph-edit :bind "is_minimap_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (graph-edit+set-show-menu :class 'graph-edit :bind "set_show_menu" :hash
  2586408642)
 :void (hidden bool))

(defgmethod
 (graph-edit+is-showing-menu :class 'graph-edit :bind "is_showing_menu" :hash
  36873697)
 bool)

(defgmethod
 (graph-edit+set-show-zoom-label :class 'graph-edit :bind "set_show_zoom_label"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (graph-edit+is-showing-zoom-label :class 'graph-edit :bind
  "is_showing_zoom_label" :hash 36873697)
 bool)

(defgmethod
 (graph-edit+set-show-grid-buttons :class 'graph-edit :bind
  "set_show_grid_buttons" :hash 2586408642)
 :void (hidden bool))

(defgmethod
 (graph-edit+is-showing-grid-buttons :class 'graph-edit :bind
  "is_showing_grid_buttons" :hash 36873697)
 bool)

(defgmethod
 (graph-edit+set-show-zoom-buttons :class 'graph-edit :bind
  "set_show_zoom_buttons" :hash 2586408642)
 :void (hidden bool))

(defgmethod
 (graph-edit+is-showing-zoom-buttons :class 'graph-edit :bind
  "is_showing_zoom_buttons" :hash 36873697)
 bool)

(defgmethod
 (graph-edit+set-show-minimap-button :class 'graph-edit :bind
  "set_show_minimap_button" :hash 2586408642)
 :void (hidden bool))

(defgmethod
 (graph-edit+is-showing-minimap-button :class 'graph-edit :bind
  "is_showing_minimap_button" :hash 36873697)
 bool)

(defgmethod
 (graph-edit+set-show-arrange-button :class 'graph-edit :bind
  "set_show_arrange_button" :hash 2586408642)
 :void (hidden bool))

(defgmethod
 (graph-edit+is-showing-arrange-button :class 'graph-edit :bind
  "is_showing_arrange_button" :hash 36873697)
 bool)

(defgmethod
 (graph-edit+set-right-disconnects :class 'graph-edit :bind
  "set_right_disconnects" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (graph-edit+is-right-disconnects-enabled :class 'graph-edit :bind
  "is_right_disconnects_enabled" :hash 36873697)
 bool)

(defgmethod
 (graph-edit+set-type-names :class 'graph-edit :bind "set_type_names" :hash
  4155329257)
 :void (type-names dictionary))

(defgmethod
 (graph-edit+get-type-names :class 'graph-edit :bind "get_type_names" :hash
  3102165223)
 dictionary)

(defgmethod
 (graph-edit+get-menu-hbox :class 'graph-edit :bind "get_menu_hbox" :hash
  3590609951)
 hbox-container)

(defgmethod
 (graph-edit+arrange-nodes :class 'graph-edit :bind "arrange_nodes" :hash
  3218959716)
 :void)

(defgmethod
 (graph-edit+set-selected :class 'graph-edit :bind "set_selected" :hash
  1078189570)
 :void (node node))