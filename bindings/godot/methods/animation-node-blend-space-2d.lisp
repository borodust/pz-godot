(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-blend-space-2d+add-blend-point :class
  'animation-node-blend-space-2d :bind "add_blend_point" :hash 768750458)
 :void (node animation-root-node) (pos vector-2) (at-index int)
 (name string-name))

(defgmethod
 (animation-node-blend-space-2d+set-blend-point-position :class
  'animation-node-blend-space-2d :bind "set_blend_point_position" :hash
  163021252)
 :void (point int) (pos vector-2))

(defgmethod
 (animation-node-blend-space-2d+get-blend-point-position :class
  'animation-node-blend-space-2d :bind "get_blend_point_position" :hash
  2299179447)
 vector-2 (point int))

(defgmethod
 (animation-node-blend-space-2d+set-blend-point-node :class
  'animation-node-blend-space-2d :bind "set_blend_point_node" :hash 4240341528)
 :void (point int) (node animation-root-node))

(defgmethod
 (animation-node-blend-space-2d+get-blend-point-node :class
  'animation-node-blend-space-2d :bind "get_blend_point_node" :hash 665599029)
 animation-root-node (point int))

(defgmethod
 (animation-node-blend-space-2d+set-blend-point-name :class
  'animation-node-blend-space-2d :bind "set_blend_point_name" :hash 3780747571)
 :void (point int) (name string-name))

(defgmethod
 (animation-node-blend-space-2d+get-blend-point-name :class
  'animation-node-blend-space-2d :bind "get_blend_point_name" :hash 659327637)
 string-name (point int))

(defgmethod
 (animation-node-blend-space-2d+find-blend-point-by-name :class
  'animation-node-blend-space-2d :bind "find_blend_point_by_name" :hash
  2458036349)
 int (name string-name))

(defgmethod
 (animation-node-blend-space-2d+remove-blend-point :class
  'animation-node-blend-space-2d :bind "remove_blend_point" :hash 1286410249)
 :void (point int))

(defgmethod
 (animation-node-blend-space-2d+get-blend-point-count :class
  'animation-node-blend-space-2d :bind "get_blend_point_count" :hash
  3905245786)
 int)

(defgmethod
 (animation-node-blend-space-2d+reorder-blend-point :class
  'animation-node-blend-space-2d :bind "reorder_blend_point" :hash 3937882851)
 :void (from-index int) (to-index int))

(defgmethod
 (animation-node-blend-space-2d+add-triangle :class
  'animation-node-blend-space-2d :bind "add_triangle" :hash 753017335)
 :void (x int) (y int) (z int) (at-index int))

(defgmethod
 (animation-node-blend-space-2d+get-triangle-point :class
  'animation-node-blend-space-2d :bind "get_triangle_point" :hash 50157827)
 int (triangle int) (point int))

(defgmethod
 (animation-node-blend-space-2d+remove-triangle :class
  'animation-node-blend-space-2d :bind "remove_triangle" :hash 1286410249)
 :void (triangle int))

(defgmethod
 (animation-node-blend-space-2d+get-triangle-count :class
  'animation-node-blend-space-2d :bind "get_triangle_count" :hash 3905245786)
 int)

(defgmethod
 (animation-node-blend-space-2d+set-min-space :class
  'animation-node-blend-space-2d :bind "set_min_space" :hash 743155724)
 :void (min-space vector-2))

(defgmethod
 (animation-node-blend-space-2d+get-min-space :class
  'animation-node-blend-space-2d :bind "get_min_space" :hash 3341600327)
 vector-2)

(defgmethod
 (animation-node-blend-space-2d+set-max-space :class
  'animation-node-blend-space-2d :bind "set_max_space" :hash 743155724)
 :void (max-space vector-2))

(defgmethod
 (animation-node-blend-space-2d+get-max-space :class
  'animation-node-blend-space-2d :bind "get_max_space" :hash 3341600327)
 vector-2)

(defgmethod
 (animation-node-blend-space-2d+set-snap :class 'animation-node-blend-space-2d
  :bind "set_snap" :hash 743155724)
 :void (snap vector-2))

(defgmethod
 (animation-node-blend-space-2d+get-snap :class 'animation-node-blend-space-2d
  :bind "get_snap" :hash 3341600327)
 vector-2)

(defgmethod
 (animation-node-blend-space-2d+set-x-label :class
  'animation-node-blend-space-2d :bind "set_x_label" :hash 83702148)
 :void (text string))

(defgmethod
 (animation-node-blend-space-2d+get-x-label :class
  'animation-node-blend-space-2d :bind "get_x_label" :hash 201670096)
 string)

(defgmethod
 (animation-node-blend-space-2d+set-y-label :class
  'animation-node-blend-space-2d :bind "set_y_label" :hash 83702148)
 :void (text string))

(defgmethod
 (animation-node-blend-space-2d+get-y-label :class
  'animation-node-blend-space-2d :bind "get_y_label" :hash 201670096)
 string)

(defgmethod
 (animation-node-blend-space-2d+set-auto-triangles :class
  'animation-node-blend-space-2d :bind "set_auto_triangles" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-blend-space-2d+get-auto-triangles :class
  'animation-node-blend-space-2d :bind "get_auto_triangles" :hash 36873697)
 bool)

(defgmethod
 (animation-node-blend-space-2d+set-blend-mode :class
  'animation-node-blend-space-2d :bind "set_blend_mode" :hash 81193520)
 :void (mode animation-node-blend-space-2d+blend-mode))

(defgmethod
 (animation-node-blend-space-2d+get-blend-mode :class
  'animation-node-blend-space-2d :bind "get_blend_mode" :hash 1398433632)
 animation-node-blend-space-2d+blend-mode)

(defgmethod
 (animation-node-blend-space-2d+set-use-sync :class
  'animation-node-blend-space-2d :bind "set_use_sync" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-blend-space-2d+is-using-sync :class
  'animation-node-blend-space-2d :bind "is_using_sync" :hash 36873697)
 bool)

(defgmethod
 (animation-node-blend-space-2d+set-sync-mode :class
  'animation-node-blend-space-2d :bind "set_sync_mode" :hash 2615784488)
 :void (sync-mode animation-node-blend-space-2d+sync-mode))

(defgmethod
 (animation-node-blend-space-2d+get-sync-mode :class
  'animation-node-blend-space-2d :bind "get_sync_mode" :hash 242032665)
 animation-node-blend-space-2d+sync-mode)

(defgmethod
 (animation-node-blend-space-2d+set-cyclic-length :class
  'animation-node-blend-space-2d :bind "set_cyclic_length" :hash 373806689)
 :void (length float))

(defgmethod
 (animation-node-blend-space-2d+get-cyclic-length :class
  'animation-node-blend-space-2d :bind "get_cyclic_length" :hash 1740695150)
 float)