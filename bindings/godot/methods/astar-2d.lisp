(common-lisp:in-package :%godot)


(defgmethod
 (astar-2d+-filter-neighbor :class 'astar-2d :bind "_filter_neighbor" :hash
  2522259332 :virtual common-lisp:t)
 bool (from-id int) (neighbor-id int))

(defgmethod
 (astar-2d+-estimate-cost :class 'astar-2d :bind "_estimate_cost" :hash
  3085491603 :virtual common-lisp:t)
 float (from-id int) (end-id int))

(defgmethod
 (astar-2d+-compute-cost :class 'astar-2d :bind "_compute_cost" :hash
  3085491603 :virtual common-lisp:t)
 float (from-id int) (to-id int))

(defgmethod
 (astar-2d+get-available-point-id :class 'astar-2d :bind
  "get_available_point_id" :hash 3905245786)
 int)

(defgmethod
 (astar-2d+add-point :class 'astar-2d :bind "add_point" :hash 4074201818) :void
 (id int) (position vector-2) (weight-scale float))

(defgmethod
 (astar-2d+get-point-position :class 'astar-2d :bind "get_point_position" :hash
  2299179447)
 vector-2 (id int))

(defgmethod
 (astar-2d+set-point-position :class 'astar-2d :bind "set_point_position" :hash
  163021252)
 :void (id int) (position vector-2))

(defgmethod
 (astar-2d+get-point-weight-scale :class 'astar-2d :bind
  "get_point_weight_scale" :hash 2339986948)
 float (id int))

(defgmethod
 (astar-2d+set-point-weight-scale :class 'astar-2d :bind
  "set_point_weight_scale" :hash 1602489585)
 :void (id int) (weight-scale float))

(defgmethod
 (astar-2d+remove-point :class 'astar-2d :bind "remove_point" :hash 1286410249)
 :void (id int))

(defgmethod
 (astar-2d+has-point :class 'astar-2d :bind "has_point" :hash 1116898809) bool
 (id int))

(defgmethod
 (astar-2d+get-point-connections :class 'astar-2d :bind "get_point_connections"
  :hash 2865087369)
 packed-int-64array (id int))

(defgmethod
 (astar-2d+get-point-ids :class 'astar-2d :bind "get_point_ids" :hash
  3851388692)
 packed-int-64array)

(defgmethod
 (astar-2d+set-neighbor-filter-enabled :class 'astar-2d :bind
  "set_neighbor_filter_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (astar-2d+is-neighbor-filter-enabled :class 'astar-2d :bind
  "is_neighbor_filter_enabled" :hash 36873697)
 bool)

(defgmethod
 (astar-2d+set-point-disabled :class 'astar-2d :bind "set_point_disabled" :hash
  972357352)
 :void (id int) (disabled bool))

(defgmethod
 (astar-2d+is-point-disabled :class 'astar-2d :bind "is_point_disabled" :hash
  1116898809)
 bool (id int))

(defgmethod
 (astar-2d+connect-points :class 'astar-2d :bind "connect_points" :hash
  3710494224)
 :void (id int) (to-id int) (bidirectional bool))

(defgmethod
 (astar-2d+disconnect-points :class 'astar-2d :bind "disconnect_points" :hash
  3710494224)
 :void (id int) (to-id int) (bidirectional bool))

(defgmethod
 (astar-2d+are-points-connected :class 'astar-2d :bind "are_points_connected"
  :hash 2288175859)
 bool (id int) (to-id int) (bidirectional bool))

(defgmethod
 (astar-2d+get-point-count :class 'astar-2d :bind "get_point_count" :hash
  3905245786)
 int)

(defgmethod
 (astar-2d+get-point-capacity :class 'astar-2d :bind "get_point_capacity" :hash
  3905245786)
 int)

(defgmethod
 (astar-2d+reserve-space :class 'astar-2d :bind "reserve_space" :hash
  1286410249)
 :void (num-nodes int))

(defgmethod (astar-2d+clear :class 'astar-2d :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (astar-2d+get-closest-point :class 'astar-2d :bind "get_closest_point" :hash
  2300324924)
 int (to-position vector-2) (include-disabled bool))

(defgmethod
 (astar-2d+get-closest-position-in-segment :class 'astar-2d :bind
  "get_closest_position_in_segment" :hash 2656412154)
 vector-2 (to-position vector-2))

(defgmethod
 (astar-2d+get-point-path :class 'astar-2d :bind "get_point_path" :hash
  3427490392)
 packed-vector-2array (from-id int) (to-id int) (allow-partial-path bool))

(defgmethod
 (astar-2d+get-id-path :class 'astar-2d :bind "get_id_path" :hash 3136199648)
 packed-int-64array (from-id int) (to-id int) (allow-partial-path bool))