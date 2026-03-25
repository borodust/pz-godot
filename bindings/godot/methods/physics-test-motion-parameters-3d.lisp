(common-lisp:in-package :%godot)


(defgmethod
 (physics-test-motion-parameters-3d+get-from :class
  'physics-test-motion-parameters-3d :bind "get_from" :hash 3229777777)
 transform-3d)

(defgmethod
 (physics-test-motion-parameters-3d+set-from :class
  'physics-test-motion-parameters-3d :bind "set_from" :hash 2952846383)
 :void (from transform-3d))

(defgmethod
 (physics-test-motion-parameters-3d+get-motion :class
  'physics-test-motion-parameters-3d :bind "get_motion" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-test-motion-parameters-3d+set-motion :class
  'physics-test-motion-parameters-3d :bind "set_motion" :hash 3460891852)
 :void (motion vector-3))

(defgmethod
 (physics-test-motion-parameters-3d+get-margin :class
  'physics-test-motion-parameters-3d :bind "get_margin" :hash 1740695150)
 float)

(defgmethod
 (physics-test-motion-parameters-3d+set-margin :class
  'physics-test-motion-parameters-3d :bind "set_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (physics-test-motion-parameters-3d+get-max-collisions :class
  'physics-test-motion-parameters-3d :bind "get_max_collisions" :hash
  3905245786)
 int)

(defgmethod
 (physics-test-motion-parameters-3d+set-max-collisions :class
  'physics-test-motion-parameters-3d :bind "set_max_collisions" :hash
  1286410249)
 :void (max-collisions int))

(defgmethod
 (physics-test-motion-parameters-3d+is-collide-separation-ray-enabled :class
  'physics-test-motion-parameters-3d :bind "is_collide_separation_ray_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (physics-test-motion-parameters-3d+set-collide-separation-ray-enabled :class
  'physics-test-motion-parameters-3d :bind "set_collide_separation_ray_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (physics-test-motion-parameters-3d+get-exclude-bodies :class
  'physics-test-motion-parameters-3d :bind "get_exclude_bodies" :hash
  3995934104)
 array)

(defgmethod
 (physics-test-motion-parameters-3d+set-exclude-bodies :class
  'physics-test-motion-parameters-3d :bind "set_exclude_bodies" :hash
  381264803)
 :void (exclude-list array))

(defgmethod
 (physics-test-motion-parameters-3d+get-exclude-objects :class
  'physics-test-motion-parameters-3d :bind "get_exclude_objects" :hash
  3995934104)
 array)

(defgmethod
 (physics-test-motion-parameters-3d+set-exclude-objects :class
  'physics-test-motion-parameters-3d :bind "set_exclude_objects" :hash
  381264803)
 :void (exclude-list array))

(defgmethod
 (physics-test-motion-parameters-3d+is-recovery-as-collision-enabled :class
  'physics-test-motion-parameters-3d :bind "is_recovery_as_collision_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (physics-test-motion-parameters-3d+set-recovery-as-collision-enabled :class
  'physics-test-motion-parameters-3d :bind "set_recovery_as_collision_enabled"
  :hash 2586408642)
 :void (enabled bool))