(common-lisp:in-package :%godot)


(defgmethod
 (physics-test-motion-result-3d+get-travel :class
  'physics-test-motion-result-3d :bind "get_travel" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-test-motion-result-3d+get-remainder :class
  'physics-test-motion-result-3d :bind "get_remainder" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-test-motion-result-3d+get-collision-safe-fraction :class
  'physics-test-motion-result-3d :bind "get_collision_safe_fraction" :hash
  1740695150)
 float)

(defgmethod
 (physics-test-motion-result-3d+get-collision-unsafe-fraction :class
  'physics-test-motion-result-3d :bind "get_collision_unsafe_fraction" :hash
  1740695150)
 float)

(defgmethod
 (physics-test-motion-result-3d+get-collision-count :class
  'physics-test-motion-result-3d :bind "get_collision_count" :hash 3905245786)
 int)

(defgmethod
 (physics-test-motion-result-3d+get-collision-point :class
  'physics-test-motion-result-3d :bind "get_collision_point" :hash 1914908202)
 vector-3 (collision-index int))

(defgmethod
 (physics-test-motion-result-3d+get-collision-normal :class
  'physics-test-motion-result-3d :bind "get_collision_normal" :hash 1914908202)
 vector-3 (collision-index int))

(defgmethod
 (physics-test-motion-result-3d+get-collider-velocity :class
  'physics-test-motion-result-3d :bind "get_collider_velocity" :hash
  1914908202)
 vector-3 (collision-index int))

(defgmethod
 (physics-test-motion-result-3d+get-collider-id :class
  'physics-test-motion-result-3d :bind "get_collider_id" :hash 1591665591)
 int (collision-index int))

(defgmethod
 (physics-test-motion-result-3d+get-collider-rid :class
  'physics-test-motion-result-3d :bind "get_collider_rid" :hash 1231817359)
 rid (collision-index int))

(defgmethod
 (physics-test-motion-result-3d+get-collider :class
  'physics-test-motion-result-3d :bind "get_collider" :hash 2639523548)
 object (collision-index int))

(defgmethod
 (physics-test-motion-result-3d+get-collider-shape :class
  'physics-test-motion-result-3d :bind "get_collider_shape" :hash 1591665591)
 int (collision-index int))

(defgmethod
 (physics-test-motion-result-3d+get-collision-local-shape :class
  'physics-test-motion-result-3d :bind "get_collision_local_shape" :hash
  1591665591)
 int (collision-index int))

(defgmethod
 (physics-test-motion-result-3d+get-collision-depth :class
  'physics-test-motion-result-3d :bind "get_collision_depth" :hash 218038398)
 float (collision-index int))