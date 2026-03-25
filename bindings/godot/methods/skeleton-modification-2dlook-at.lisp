(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-modification-2dlook-at+set-bone2d-node :class
  'skeleton-modification-2dlook-at :bind "set_bone2d_node" :hash 1348162250)
 :void (bone2d-nodepath node-path))

(defgmethod
 (skeleton-modification-2dlook-at+get-bone2d-node :class
  'skeleton-modification-2dlook-at :bind "get_bone2d_node" :hash 4075236667)
 node-path)

(defgmethod
 (skeleton-modification-2dlook-at+set-bone-index :class
  'skeleton-modification-2dlook-at :bind "set_bone_index" :hash 1286410249)
 :void (bone-idx int))

(defgmethod
 (skeleton-modification-2dlook-at+get-bone-index :class
  'skeleton-modification-2dlook-at :bind "get_bone_index" :hash 3905245786)
 int)

(defgmethod
 (skeleton-modification-2dlook-at+set-target-node :class
  'skeleton-modification-2dlook-at :bind "set_target_node" :hash 1348162250)
 :void (target-nodepath node-path))

(defgmethod
 (skeleton-modification-2dlook-at+get-target-node :class
  'skeleton-modification-2dlook-at :bind "get_target_node" :hash 4075236667)
 node-path)

(defgmethod
 (skeleton-modification-2dlook-at+set-additional-rotation :class
  'skeleton-modification-2dlook-at :bind "set_additional_rotation" :hash
  373806689)
 :void (rotation float))

(defgmethod
 (skeleton-modification-2dlook-at+get-additional-rotation :class
  'skeleton-modification-2dlook-at :bind "get_additional_rotation" :hash
  1740695150)
 float)

(defgmethod
 (skeleton-modification-2dlook-at+set-enable-constraint :class
  'skeleton-modification-2dlook-at :bind "set_enable_constraint" :hash
  2586408642)
 :void (enable-constraint bool))

(defgmethod
 (skeleton-modification-2dlook-at+get-enable-constraint :class
  'skeleton-modification-2dlook-at :bind "get_enable_constraint" :hash
  36873697)
 bool)

(defgmethod
 (skeleton-modification-2dlook-at+set-constraint-angle-min :class
  'skeleton-modification-2dlook-at :bind "set_constraint_angle_min" :hash
  373806689)
 :void (angle-min float))

(defgmethod
 (skeleton-modification-2dlook-at+get-constraint-angle-min :class
  'skeleton-modification-2dlook-at :bind "get_constraint_angle_min" :hash
  1740695150)
 float)

(defgmethod
 (skeleton-modification-2dlook-at+set-constraint-angle-max :class
  'skeleton-modification-2dlook-at :bind "set_constraint_angle_max" :hash
  373806689)
 :void (angle-max float))

(defgmethod
 (skeleton-modification-2dlook-at+get-constraint-angle-max :class
  'skeleton-modification-2dlook-at :bind "get_constraint_angle_max" :hash
  1740695150)
 float)

(defgmethod
 (skeleton-modification-2dlook-at+set-constraint-angle-invert :class
  'skeleton-modification-2dlook-at :bind "set_constraint_angle_invert" :hash
  2586408642)
 :void (invert bool))

(defgmethod
 (skeleton-modification-2dlook-at+get-constraint-angle-invert :class
  'skeleton-modification-2dlook-at :bind "get_constraint_angle_invert" :hash
  36873697)
 bool)