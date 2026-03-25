(common-lisp:in-package :%godot)


(defgmethod
 (kinematic-collision-2d+get-position :class 'kinematic-collision-2d :bind
  "get_position" :hash 3341600327)
 vector-2)

(defgmethod
 (kinematic-collision-2d+get-normal :class 'kinematic-collision-2d :bind
  "get_normal" :hash 3341600327)
 vector-2)

(defgmethod
 (kinematic-collision-2d+get-travel :class 'kinematic-collision-2d :bind
  "get_travel" :hash 3341600327)
 vector-2)

(defgmethod
 (kinematic-collision-2d+get-remainder :class 'kinematic-collision-2d :bind
  "get_remainder" :hash 3341600327)
 vector-2)

(defgmethod
 (kinematic-collision-2d+get-angle :class 'kinematic-collision-2d :bind
  "get_angle" :hash 2841063350)
 float (up-direction vector-2))

(defgmethod
 (kinematic-collision-2d+get-depth :class 'kinematic-collision-2d :bind
  "get_depth" :hash 1740695150)
 float)

(defgmethod
 (kinematic-collision-2d+get-local-shape :class 'kinematic-collision-2d :bind
  "get_local_shape" :hash 1981248198)
 object)

(defgmethod
 (kinematic-collision-2d+get-collider :class 'kinematic-collision-2d :bind
  "get_collider" :hash 1981248198)
 object)

(defgmethod
 (kinematic-collision-2d+get-collider-id :class 'kinematic-collision-2d :bind
  "get_collider_id" :hash 3905245786)
 int)

(defgmethod
 (kinematic-collision-2d+get-collider-rid :class 'kinematic-collision-2d :bind
  "get_collider_rid" :hash 2944877500)
 rid)

(defgmethod
 (kinematic-collision-2d+get-collider-shape :class 'kinematic-collision-2d
  :bind "get_collider_shape" :hash 1981248198)
 object)

(defgmethod
 (kinematic-collision-2d+get-collider-shape-index :class
  'kinematic-collision-2d :bind "get_collider_shape_index" :hash 3905245786)
 int)

(defgmethod
 (kinematic-collision-2d+get-collider-velocity :class 'kinematic-collision-2d
  :bind "get_collider_velocity" :hash 3341600327)
 vector-2)