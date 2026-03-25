(common-lisp:in-package :%godot)


(defgmethod
 (physics-body-3d+move-and-collide :class 'physics-body-3d :bind
  "move_and_collide" :hash 3208792678)
 kinematic-collision-3d (motion vector-3) (test-only bool) (safe-margin float)
 (recovery-as-collision bool) (max-collisions int))

(defgmethod
 (physics-body-3d+test-move :class 'physics-body-3d :bind "test_move" :hash
  2481691619)
 bool (from transform-3d) (motion vector-3) (collision kinematic-collision-3d)
 (safe-margin float) (recovery-as-collision bool) (max-collisions int))

(defgmethod
 (physics-body-3d+get-gravity :class 'physics-body-3d :bind "get_gravity" :hash
  3360562783)
 vector-3)

(defgmethod
 (physics-body-3d+set-axis-lock :class 'physics-body-3d :bind "set_axis_lock"
  :hash 1787895195)
 :void (axis physics-server-3d+body-axis) (lock bool))

(defgmethod
 (physics-body-3d+get-axis-lock :class 'physics-body-3d :bind "get_axis_lock"
  :hash 2264617709)
 bool (axis physics-server-3d+body-axis))

(defgmethod
 (physics-body-3d+get-collision-exceptions :class 'physics-body-3d :bind
  "get_collision_exceptions" :hash 2915620761)
 array)

(defgmethod
 (physics-body-3d+add-collision-exception-with :class 'physics-body-3d :bind
  "add_collision_exception_with" :hash 1078189570)
 :void (body node))

(defgmethod
 (physics-body-3d+remove-collision-exception-with :class 'physics-body-3d :bind
  "remove_collision_exception_with" :hash 1078189570)
 :void (body node))