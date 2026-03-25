(common-lisp:in-package :%godot)


(defgmethod
 (physical-bone-simulator-3d+is-simulating-physics :class
  'physical-bone-simulator-3d :bind "is_simulating_physics" :hash 36873697)
 bool)

(defgmethod
 (physical-bone-simulator-3d+physical-bones-stop-simulation :class
  'physical-bone-simulator-3d :bind "physical_bones_stop_simulation" :hash
  3218959716)
 :void)

(defgmethod
 (physical-bone-simulator-3d+physical-bones-start-simulation :class
  'physical-bone-simulator-3d :bind "physical_bones_start_simulation" :hash
  2787316981)
 :void (bones array))

(defgmethod
 (physical-bone-simulator-3d+physical-bones-add-collision-exception :class
  'physical-bone-simulator-3d :bind "physical_bones_add_collision_exception"
  :hash 2722037293)
 :void (exception rid))

(defgmethod
 (physical-bone-simulator-3d+physical-bones-remove-collision-exception :class
  'physical-bone-simulator-3d :bind "physical_bones_remove_collision_exception"
  :hash 2722037293)
 :void (exception rid))