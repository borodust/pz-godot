(common-lisp:in-package :%godot)


(defgmethod
 (physics-test-motion-parameters-2d+get-from :class
  'physics-test-motion-parameters-2d :bind "get_from" :hash 3814499831)
 transform-2d)

(defgmethod
 (physics-test-motion-parameters-2d+set-from :class
  'physics-test-motion-parameters-2d :bind "set_from" :hash 2761652528)
 :void (from transform-2d))

(defgmethod
 (physics-test-motion-parameters-2d+get-motion :class
  'physics-test-motion-parameters-2d :bind "get_motion" :hash 3341600327)
 vector-2)

(defgmethod
 (physics-test-motion-parameters-2d+set-motion :class
  'physics-test-motion-parameters-2d :bind "set_motion" :hash 743155724)
 :void (motion vector-2))

(defgmethod
 (physics-test-motion-parameters-2d+get-margin :class
  'physics-test-motion-parameters-2d :bind "get_margin" :hash 1740695150)
 float)

(defgmethod
 (physics-test-motion-parameters-2d+set-margin :class
  'physics-test-motion-parameters-2d :bind "set_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (physics-test-motion-parameters-2d+is-collide-separation-ray-enabled :class
  'physics-test-motion-parameters-2d :bind "is_collide_separation_ray_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (physics-test-motion-parameters-2d+set-collide-separation-ray-enabled :class
  'physics-test-motion-parameters-2d :bind "set_collide_separation_ray_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (physics-test-motion-parameters-2d+get-exclude-bodies :class
  'physics-test-motion-parameters-2d :bind "get_exclude_bodies" :hash
  3995934104)
 array)

(defgmethod
 (physics-test-motion-parameters-2d+set-exclude-bodies :class
  'physics-test-motion-parameters-2d :bind "set_exclude_bodies" :hash
  381264803)
 :void (exclude-list array))

(defgmethod
 (physics-test-motion-parameters-2d+get-exclude-objects :class
  'physics-test-motion-parameters-2d :bind "get_exclude_objects" :hash
  3995934104)
 array)

(defgmethod
 (physics-test-motion-parameters-2d+set-exclude-objects :class
  'physics-test-motion-parameters-2d :bind "set_exclude_objects" :hash
  381264803)
 :void (exclude-list array))

(defgmethod
 (physics-test-motion-parameters-2d+is-recovery-as-collision-enabled :class
  'physics-test-motion-parameters-2d :bind "is_recovery_as_collision_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (physics-test-motion-parameters-2d+set-recovery-as-collision-enabled :class
  'physics-test-motion-parameters-2d :bind "set_recovery_as_collision_enabled"
  :hash 2586408642)
 :void (enabled bool))