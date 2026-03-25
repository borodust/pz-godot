(common-lisp:in-package :%godot)


(defgmethod
 (physics-body-2d+move-and-collide :class 'physics-body-2d :bind
  "move_and_collide" :hash 3681923724)
 kinematic-collision-2d (motion vector-2) (test-only bool) (safe-margin float)
 (recovery-as-collision bool))

(defgmethod
 (physics-body-2d+test-move :class 'physics-body-2d :bind "test_move" :hash
  3324464701)
 bool (from transform-2d) (motion vector-2) (collision kinematic-collision-2d)
 (safe-margin float) (recovery-as-collision bool))

(defgmethod
 (physics-body-2d+get-gravity :class 'physics-body-2d :bind "get_gravity" :hash
  3341600327)
 vector-2)

(defgmethod
 (physics-body-2d+get-collision-exceptions :class 'physics-body-2d :bind
  "get_collision_exceptions" :hash 2915620761)
 array)

(defgmethod
 (physics-body-2d+add-collision-exception-with :class 'physics-body-2d :bind
  "add_collision_exception_with" :hash 1078189570)
 :void (body node))

(defgmethod
 (physics-body-2d+remove-collision-exception-with :class 'physics-body-2d :bind
  "remove_collision_exception_with" :hash 1078189570)
 :void (body node))