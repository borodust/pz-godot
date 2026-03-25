(common-lisp:in-package :%godot)


(defgmethod
 (grid-map-editor-plugin+get-current-grid-map :class 'grid-map-editor-plugin
  :bind "get_current_grid_map" :hash 1184264483)
 grid-map)

(defgmethod
 (grid-map-editor-plugin+set-selection :class 'grid-map-editor-plugin :bind
  "set_selection" :hash 3659408297)
 :void (begin vector-3i) (end vector-3i))

(defgmethod
 (grid-map-editor-plugin+clear-selection :class 'grid-map-editor-plugin :bind
  "clear_selection" :hash 3218959716)
 :void)

(defgmethod
 (grid-map-editor-plugin+get-selection :class 'grid-map-editor-plugin :bind
  "get_selection" :hash 1068685055)
 aabb)

(defgmethod
 (grid-map-editor-plugin+has-selection :class 'grid-map-editor-plugin :bind
  "has_selection" :hash 36873697)
 bool)

(defgmethod
 (grid-map-editor-plugin+get-selected-cells :class 'grid-map-editor-plugin
  :bind "get_selected_cells" :hash 3995934104)
 array)

(defgmethod
 (grid-map-editor-plugin+set-selected-palette-item :class
  'grid-map-editor-plugin :bind "set_selected_palette_item" :hash 998575451)
 :void (item int))

(defgmethod
 (grid-map-editor-plugin+get-selected-palette-item :class
  'grid-map-editor-plugin :bind "get_selected_palette_item" :hash 3905245786)
 int)