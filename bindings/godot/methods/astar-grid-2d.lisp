(common-lisp:in-package :%godot)


(defgmethod
 (astar-grid-2d+%estimate-cost :class 'astar-grid-2d :bind "_estimate_cost"
  :hash 2153177966 :virtual common-lisp:t)
 float (from-id vector-2i) (end-id vector-2i))

(defgmethod
 (astar-grid-2d+%compute-cost :class 'astar-grid-2d :bind "_compute_cost" :hash
  2153177966 :virtual common-lisp:t)
 float (from-id vector-2i) (to-id vector-2i))

(defgmethod
 (astar-grid-2d+set-region :class 'astar-grid-2d :bind "set_region" :hash
  1763793166)
 :void (region rect-2i))

(defgmethod
 (astar-grid-2d+get-region :class 'astar-grid-2d :bind "get_region" :hash
  410525958)
 rect-2i)

(defgmethod
 (astar-grid-2d+set-size :class 'astar-grid-2d :bind "set_size" :hash
  1130785943)
 :void (size vector-2i))

(defgmethod
 (astar-grid-2d+get-size :class 'astar-grid-2d :bind "get_size" :hash
  3690982128)
 vector-2i)

(defgmethod
 (astar-grid-2d+set-offset :class 'astar-grid-2d :bind "set_offset" :hash
  743155724)
 :void (offset vector-2))

(defgmethod
 (astar-grid-2d+get-offset :class 'astar-grid-2d :bind "get_offset" :hash
  3341600327)
 vector-2)

(defgmethod
 (astar-grid-2d+set-cell-size :class 'astar-grid-2d :bind "set_cell_size" :hash
  743155724)
 :void (cell-size vector-2))

(defgmethod
 (astar-grid-2d+get-cell-size :class 'astar-grid-2d :bind "get_cell_size" :hash
  3341600327)
 vector-2)

(defgmethod
 (astar-grid-2d+set-cell-shape :class 'astar-grid-2d :bind "set_cell_shape"
  :hash 4130591146)
 :void (cell-shape astar-grid-2d+cell-shape))

(defgmethod
 (astar-grid-2d+get-cell-shape :class 'astar-grid-2d :bind "get_cell_shape"
  :hash 3293463634)
 astar-grid-2d+cell-shape)

(defgmethod
 (astar-grid-2d+is-in-bounds :class 'astar-grid-2d :bind "is_in_bounds" :hash
  2522259332)
 bool (x int) (y int))

(defgmethod
 (astar-grid-2d+is-in-boundsv :class 'astar-grid-2d :bind "is_in_boundsv" :hash
  3900751641)
 bool (id vector-2i))

(defgmethod
 (astar-grid-2d+is-dirty :class 'astar-grid-2d :bind "is_dirty" :hash 36873697)
 bool)

(defgmethod
 (astar-grid-2d+update :class 'astar-grid-2d :bind "update" :hash 3218959716)
 :void)

(defgmethod
 (astar-grid-2d+set-jumping-enabled :class 'astar-grid-2d :bind
  "set_jumping_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (astar-grid-2d+is-jumping-enabled :class 'astar-grid-2d :bind
  "is_jumping_enabled" :hash 36873697)
 bool)

(defgmethod
 (astar-grid-2d+set-diagonal-mode :class 'astar-grid-2d :bind
  "set_diagonal_mode" :hash 1017829798)
 :void (mode astar-grid-2d+diagonal-mode))

(defgmethod
 (astar-grid-2d+get-diagonal-mode :class 'astar-grid-2d :bind
  "get_diagonal_mode" :hash 3129282674)
 astar-grid-2d+diagonal-mode)

(defgmethod
 (astar-grid-2d+set-default-compute-heuristic :class 'astar-grid-2d :bind
  "set_default_compute_heuristic" :hash 1044375519)
 :void (heuristic astar-grid-2d+heuristic))

(defgmethod
 (astar-grid-2d+get-default-compute-heuristic :class 'astar-grid-2d :bind
  "get_default_compute_heuristic" :hash 2074731422)
 astar-grid-2d+heuristic)

(defgmethod
 (astar-grid-2d+set-default-estimate-heuristic :class 'astar-grid-2d :bind
  "set_default_estimate_heuristic" :hash 1044375519)
 :void (heuristic astar-grid-2d+heuristic))

(defgmethod
 (astar-grid-2d+get-default-estimate-heuristic :class 'astar-grid-2d :bind
  "get_default_estimate_heuristic" :hash 2074731422)
 astar-grid-2d+heuristic)

(defgmethod
 (astar-grid-2d+set-point-solid :class 'astar-grid-2d :bind "set_point_solid"
  :hash 1765703753)
 :void (id vector-2i) (solid bool))

(defgmethod
 (astar-grid-2d+is-point-solid :class 'astar-grid-2d :bind "is_point_solid"
  :hash 3900751641)
 bool (id vector-2i))

(defgmethod
 (astar-grid-2d+set-point-weight-scale :class 'astar-grid-2d :bind
  "set_point_weight_scale" :hash 2262553149)
 :void (id vector-2i) (weight-scale float))

(defgmethod
 (astar-grid-2d+get-point-weight-scale :class 'astar-grid-2d :bind
  "get_point_weight_scale" :hash 719993801)
 float (id vector-2i))

(defgmethod
 (astar-grid-2d+fill-solid-region :class 'astar-grid-2d :bind
  "fill_solid_region" :hash 2261970063)
 :void (region rect-2i) (solid bool))

(defgmethod
 (astar-grid-2d+fill-weight-scale-region :class 'astar-grid-2d :bind
  "fill_weight_scale_region" :hash 2793244083)
 :void (region rect-2i) (weight-scale float))

(defgmethod
 (astar-grid-2d+clear :class 'astar-grid-2d :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (astar-grid-2d+get-point-position :class 'astar-grid-2d :bind
  "get_point_position" :hash 108438297)
 vector-2 (id vector-2i))

(defgmethod
 (astar-grid-2d+get-point-data-in-region :class 'astar-grid-2d :bind
  "get_point_data_in_region" :hash 3893818462)
 array (region rect-2i))

(defgmethod
 (astar-grid-2d+get-point-path :class 'astar-grid-2d :bind "get_point_path"
  :hash 1641925693)
 packed-vector-2array (from-id vector-2i) (to-id vector-2i)
 (allow-partial-path bool))

(defgmethod
 (astar-grid-2d+get-id-path :class 'astar-grid-2d :bind "get_id_path" :hash
  1918132273)
 array (from-id vector-2i) (to-id vector-2i) (allow-partial-path bool))