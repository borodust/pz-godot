(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-blend-space-1d+add-blend-point :class
  'animation-node-blend-space-1d :bind "add_blend_point" :hash 285050433)
 :void (node animation-root-node) (pos float) (at-index int))

(defgmethod
 (animation-node-blend-space-1d+set-blend-point-position :class
  'animation-node-blend-space-1d :bind "set_blend_point_position" :hash
  1602489585)
 :void (point int) (pos float))

(defgmethod
 (animation-node-blend-space-1d+get-blend-point-position :class
  'animation-node-blend-space-1d :bind "get_blend_point_position" :hash
  2339986948)
 float (point int))

(defgmethod
 (animation-node-blend-space-1d+set-blend-point-node :class
  'animation-node-blend-space-1d :bind "set_blend_point_node" :hash 4240341528)
 :void (point int) (node animation-root-node))

(defgmethod
 (animation-node-blend-space-1d+get-blend-point-node :class
  'animation-node-blend-space-1d :bind "get_blend_point_node" :hash 665599029)
 animation-root-node (point int))

(defgmethod
 (animation-node-blend-space-1d+remove-blend-point :class
  'animation-node-blend-space-1d :bind "remove_blend_point" :hash 1286410249)
 :void (point int))

(defgmethod
 (animation-node-blend-space-1d+get-blend-point-count :class
  'animation-node-blend-space-1d :bind "get_blend_point_count" :hash
  3905245786)
 int)

(defgmethod
 (animation-node-blend-space-1d+set-min-space :class
  'animation-node-blend-space-1d :bind "set_min_space" :hash 373806689)
 :void (min-space float))

(defgmethod
 (animation-node-blend-space-1d+get-min-space :class
  'animation-node-blend-space-1d :bind "get_min_space" :hash 1740695150)
 float)

(defgmethod
 (animation-node-blend-space-1d+set-max-space :class
  'animation-node-blend-space-1d :bind "set_max_space" :hash 373806689)
 :void (max-space float))

(defgmethod
 (animation-node-blend-space-1d+get-max-space :class
  'animation-node-blend-space-1d :bind "get_max_space" :hash 1740695150)
 float)

(defgmethod
 (animation-node-blend-space-1d+set-snap :class 'animation-node-blend-space-1d
  :bind "set_snap" :hash 373806689)
 :void (snap float))

(defgmethod
 (animation-node-blend-space-1d+get-snap :class 'animation-node-blend-space-1d
  :bind "get_snap" :hash 1740695150)
 float)

(defgmethod
 (animation-node-blend-space-1d+set-value-label :class
  'animation-node-blend-space-1d :bind "set_value_label" :hash 83702148)
 :void (text string))

(defgmethod
 (animation-node-blend-space-1d+get-value-label :class
  'animation-node-blend-space-1d :bind "get_value_label" :hash 201670096)
 string)

(defgmethod
 (animation-node-blend-space-1d+set-blend-mode :class
  'animation-node-blend-space-1d :bind "set_blend_mode" :hash 2600869457)
 :void (mode animation-node-blend-space-1d+blend-mode))

(defgmethod
 (animation-node-blend-space-1d+get-blend-mode :class
  'animation-node-blend-space-1d :bind "get_blend_mode" :hash 1547667849)
 animation-node-blend-space-1d+blend-mode)

(defgmethod
 (animation-node-blend-space-1d+set-use-sync :class
  'animation-node-blend-space-1d :bind "set_use_sync" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-blend-space-1d+is-using-sync :class
  'animation-node-blend-space-1d :bind "is_using_sync" :hash 36873697)
 bool)