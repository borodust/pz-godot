(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-ik3d+set-root-bone :class 'skeleton-ik3d :bind "set_root_bone" :hash
  3304788590)
 :void (root-bone string-name))

(defgmethod
 (skeleton-ik3d+get-root-bone :class 'skeleton-ik3d :bind "get_root_bone" :hash
  2002593661)
 string-name)

(defgmethod
 (skeleton-ik3d+set-tip-bone :class 'skeleton-ik3d :bind "set_tip_bone" :hash
  3304788590)
 :void (tip-bone string-name))

(defgmethod
 (skeleton-ik3d+get-tip-bone :class 'skeleton-ik3d :bind "get_tip_bone" :hash
  2002593661)
 string-name)

(defgmethod
 (skeleton-ik3d+set-target-transform :class 'skeleton-ik3d :bind
  "set_target_transform" :hash 2952846383)
 :void (target transform-3d))

(defgmethod
 (skeleton-ik3d+get-target-transform :class 'skeleton-ik3d :bind
  "get_target_transform" :hash 3229777777)
 transform-3d)

(defgmethod
 (skeleton-ik3d+set-target-node :class 'skeleton-ik3d :bind "set_target_node"
  :hash 1348162250)
 :void (node node-path))

(defgmethod
 (skeleton-ik3d+get-target-node :class 'skeleton-ik3d :bind "get_target_node"
  :hash 277076166)
 node-path)

(defgmethod
 (skeleton-ik3d+set-override-tip-basis :class 'skeleton-ik3d :bind
  "set_override_tip_basis" :hash 2586408642)
 :void (override bool))

(defgmethod
 (skeleton-ik3d+is-override-tip-basis :class 'skeleton-ik3d :bind
  "is_override_tip_basis" :hash 36873697)
 bool)

(defgmethod
 (skeleton-ik3d+set-use-magnet :class 'skeleton-ik3d :bind "set_use_magnet"
  :hash 2586408642)
 :void (use bool))

(defgmethod
 (skeleton-ik3d+is-using-magnet :class 'skeleton-ik3d :bind "is_using_magnet"
  :hash 36873697)
 bool)

(defgmethod
 (skeleton-ik3d+set-magnet-position :class 'skeleton-ik3d :bind
  "set_magnet_position" :hash 3460891852)
 :void (local-position vector-3))

(defgmethod
 (skeleton-ik3d+get-magnet-position :class 'skeleton-ik3d :bind
  "get_magnet_position" :hash 3360562783)
 vector-3)

(defgmethod
 (skeleton-ik3d+get-parent-skeleton :class 'skeleton-ik3d :bind
  "get_parent_skeleton" :hash 1488626673)
 skeleton-3d)

(defgmethod
 (skeleton-ik3d+is-running :class 'skeleton-ik3d :bind "is_running" :hash
  2240911060)
 bool)

(defgmethod
 (skeleton-ik3d+set-min-distance :class 'skeleton-ik3d :bind "set_min_distance"
  :hash 373806689)
 :void (min-distance float))

(defgmethod
 (skeleton-ik3d+get-min-distance :class 'skeleton-ik3d :bind "get_min_distance"
  :hash 1740695150)
 float)

(defgmethod
 (skeleton-ik3d+set-max-iterations :class 'skeleton-ik3d :bind
  "set_max_iterations" :hash 1286410249)
 :void (iterations int))

(defgmethod
 (skeleton-ik3d+get-max-iterations :class 'skeleton-ik3d :bind
  "get_max_iterations" :hash 3905245786)
 int)

(defgmethod
 (skeleton-ik3d+start :class 'skeleton-ik3d :bind "start" :hash 107499316)
 :void (one-time bool))

(defgmethod
 (skeleton-ik3d+stop :class 'skeleton-ik3d :bind "stop" :hash 3218959716) :void)

(defgmethod
 (skeleton-ik3d+set-interpolation :class 'skeleton-ik3d :bind
  "set_interpolation" :hash 373806689)
 :void (interpolation float))

(defgmethod
 (skeleton-ik3d+get-interpolation :class 'skeleton-ik3d :bind
  "get_interpolation" :hash 1740695150)
 float)