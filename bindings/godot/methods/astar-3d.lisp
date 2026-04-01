(common-lisp:in-package :%godot)


(defgmethod
 (astar-3d+%filter-neighbor :class 'astar-3d :bind "_filter_neighbor" :hash
  2522259332 :virtual common-lisp:t)
 bool (from-id int) (neighbor-id int))

(defgmethod
 (astar-3d+%estimate-cost :class 'astar-3d :bind "_estimate_cost" :hash
  3085491603 :virtual common-lisp:t)
 float (from-id int) (end-id int))

(defgmethod
 (astar-3d+%compute-cost :class 'astar-3d :bind "_compute_cost" :hash
  3085491603 :virtual common-lisp:t)
 float (from-id int) (to-id int))

(defgmethod
 (astar-3d+get-available-point-id :class 'astar-3d :bind
  "get_available_point_id" :hash 3905245786)
 int)

(defgmethod
 (astar-3d+add-point :class 'astar-3d :bind "add_point" :hash 1038703438) :void
 (id int) (position vector-3) (weight-scale float))

(defgmethod
 (astar-3d+get-point-position :class 'astar-3d :bind "get_point_position" :hash
  711720468)
 vector-3 (id int))

(defgmethod
 (astar-3d+set-point-position :class 'astar-3d :bind "set_point_position" :hash
  1530502735)
 :void (id int) (position vector-3))

(defgmethod
 (astar-3d+get-point-weight-scale :class 'astar-3d :bind
  "get_point_weight_scale" :hash 2339986948)
 float (id int))

(defgmethod
 (astar-3d+set-point-weight-scale :class 'astar-3d :bind
  "set_point_weight_scale" :hash 1602489585)
 :void (id int) (weight-scale float))

(defgmethod
 (astar-3d+remove-point :class 'astar-3d :bind "remove_point" :hash 1286410249)
 :void (id int))

(defgmethod
 (astar-3d+has-point :class 'astar-3d :bind "has_point" :hash 1116898809) bool
 (id int))

(defgmethod
 (astar-3d+get-point-connections :class 'astar-3d :bind "get_point_connections"
  :hash 2865087369)
 packed-int-64array (id int))

(defgmethod
 (astar-3d+get-point-ids :class 'astar-3d :bind "get_point_ids" :hash
  3851388692)
 packed-int-64array)

(defgmethod
 (astar-3d+set-point-disabled :class 'astar-3d :bind "set_point_disabled" :hash
  972357352)
 :void (id int) (disabled bool))

(defgmethod
 (astar-3d+is-point-disabled :class 'astar-3d :bind "is_point_disabled" :hash
  1116898809)
 bool (id int))

(defgmethod
 (astar-3d+set-neighbor-filter-enabled :class 'astar-3d :bind
  "set_neighbor_filter_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (astar-3d+is-neighbor-filter-enabled :class 'astar-3d :bind
  "is_neighbor_filter_enabled" :hash 36873697)
 bool)

(defgmethod
 (astar-3d+connect-points :class 'astar-3d :bind "connect_points" :hash
  3710494224)
 :void (id int) (to-id int) (bidirectional bool))

(defgmethod
 (astar-3d+disconnect-points :class 'astar-3d :bind "disconnect_points" :hash
  3710494224)
 :void (id int) (to-id int) (bidirectional bool))

(defgmethod
 (astar-3d+are-points-connected :class 'astar-3d :bind "are_points_connected"
  :hash 2288175859)
 bool (id int) (to-id int) (bidirectional bool))

(defgmethod
 (astar-3d+get-point-count :class 'astar-3d :bind "get_point_count" :hash
  3905245786)
 int)

(defgmethod
 (astar-3d+get-point-capacity :class 'astar-3d :bind "get_point_capacity" :hash
  3905245786)
 int)

(defgmethod
 (astar-3d+reserve-space :class 'astar-3d :bind "reserve_space" :hash
  1286410249)
 :void (num-nodes int))

(defgmethod (astar-3d+clear :class 'astar-3d :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (astar-3d+get-closest-point :class 'astar-3d :bind "get_closest_point" :hash
  3241074317)
 int (to-position vector-3) (include-disabled bool))

(defgmethod
 (astar-3d+get-closest-position-in-segment :class 'astar-3d :bind
  "get_closest_position_in_segment" :hash 192990374)
 vector-3 (to-position vector-3))

(defgmethod
 (astar-3d+get-point-path :class 'astar-3d :bind "get_point_path" :hash
  1562654675)
 packed-vector-3array (from-id int) (to-id int) (allow-partial-path bool))

(defgmethod
 (astar-3d+get-id-path :class 'astar-3d :bind "get_id_path" :hash 3136199648)
 packed-int-64array (from-id int) (to-id int) (allow-partial-path bool))