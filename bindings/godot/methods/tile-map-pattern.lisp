(common-lisp:in-package :%godot)


(defgmethod
 (tile-map-pattern+set-cell :class 'tile-map-pattern :bind "set_cell" :hash
  2224802556)
 :void (coords vector-2i) (source-id int) (atlas-coords vector-2i)
 (alternative-tile int))

(defgmethod
 (tile-map-pattern+has-cell :class 'tile-map-pattern :bind "has_cell" :hash
  3900751641)
 bool (coords vector-2i))

(defgmethod
 (tile-map-pattern+remove-cell :class 'tile-map-pattern :bind "remove_cell"
  :hash 4153096796)
 :void (coords vector-2i) (update-size bool))

(defgmethod
 (tile-map-pattern+get-cell-source-id :class 'tile-map-pattern :bind
  "get_cell_source_id" :hash 2485466453)
 int (coords vector-2i))

(defgmethod
 (tile-map-pattern+get-cell-atlas-coords :class 'tile-map-pattern :bind
  "get_cell_atlas_coords" :hash 3050897911)
 vector-2i (coords vector-2i))

(defgmethod
 (tile-map-pattern+get-cell-alternative-tile :class 'tile-map-pattern :bind
  "get_cell_alternative_tile" :hash 2485466453)
 int (coords vector-2i))

(defgmethod
 (tile-map-pattern+get-used-cells :class 'tile-map-pattern :bind
  "get_used_cells" :hash 3995934104)
 array)

(defgmethod
 (tile-map-pattern+get-size :class 'tile-map-pattern :bind "get_size" :hash
  3690982128)
 vector-2i)

(defgmethod
 (tile-map-pattern+set-size :class 'tile-map-pattern :bind "set_size" :hash
  1130785943)
 :void (size vector-2i))

(defgmethod
 (tile-map-pattern+is-empty :class 'tile-map-pattern :bind "is_empty" :hash
  36873697)
 bool)