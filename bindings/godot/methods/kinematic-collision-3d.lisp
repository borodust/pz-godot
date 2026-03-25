(common-lisp:in-package :%godot)


(defgmethod
 (kinematic-collision-3d+get-travel :class 'kinematic-collision-3d :bind
  "get_travel" :hash 3360562783)
 vector-3)

(defgmethod
 (kinematic-collision-3d+get-remainder :class 'kinematic-collision-3d :bind
  "get_remainder" :hash 3360562783)
 vector-3)

(defgmethod
 (kinematic-collision-3d+get-depth :class 'kinematic-collision-3d :bind
  "get_depth" :hash 1740695150)
 float)

(defgmethod
 (kinematic-collision-3d+get-collision-count :class 'kinematic-collision-3d
  :bind "get_collision_count" :hash 3905245786)
 int)

(defgmethod
 (kinematic-collision-3d+get-position :class 'kinematic-collision-3d :bind
  "get_position" :hash 1914908202)
 vector-3 (collision-index int))

(defgmethod
 (kinematic-collision-3d+get-normal :class 'kinematic-collision-3d :bind
  "get_normal" :hash 1914908202)
 vector-3 (collision-index int))

(defgmethod
 (kinematic-collision-3d+get-angle :class 'kinematic-collision-3d :bind
  "get_angle" :hash 1242741860)
 float (collision-index int) (up-direction vector-3))

(defgmethod
 (kinematic-collision-3d+get-local-shape :class 'kinematic-collision-3d :bind
  "get_local_shape" :hash 2639523548)
 object (collision-index int))

(defgmethod
 (kinematic-collision-3d+get-collider :class 'kinematic-collision-3d :bind
  "get_collider" :hash 2639523548)
 object (collision-index int))

(defgmethod
 (kinematic-collision-3d+get-collider-id :class 'kinematic-collision-3d :bind
  "get_collider_id" :hash 1591665591)
 int (collision-index int))

(defgmethod
 (kinematic-collision-3d+get-collider-rid :class 'kinematic-collision-3d :bind
  "get_collider_rid" :hash 1231817359)
 rid (collision-index int))

(defgmethod
 (kinematic-collision-3d+get-collider-shape :class 'kinematic-collision-3d
  :bind "get_collider_shape" :hash 2639523548)
 object (collision-index int))

(defgmethod
 (kinematic-collision-3d+get-collider-shape-index :class
  'kinematic-collision-3d :bind "get_collider_shape_index" :hash 1591665591)
 int (collision-index int))

(defgmethod
 (kinematic-collision-3d+get-collider-velocity :class 'kinematic-collision-3d
  :bind "get_collider_velocity" :hash 1914908202)
 vector-3 (collision-index int))