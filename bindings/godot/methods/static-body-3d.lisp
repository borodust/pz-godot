(common-lisp:in-package :%godot)


(defgmethod
 (static-body-3d+set-constant-linear-velocity :class 'static-body-3d :bind
  "set_constant_linear_velocity" :hash 3460891852)
 :void (vel vector-3))

(defgmethod
 (static-body-3d+set-constant-angular-velocity :class 'static-body-3d :bind
  "set_constant_angular_velocity" :hash 3460891852)
 :void (vel vector-3))

(defgmethod
 (static-body-3d+get-constant-linear-velocity :class 'static-body-3d :bind
  "get_constant_linear_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (static-body-3d+get-constant-angular-velocity :class 'static-body-3d :bind
  "get_constant_angular_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (static-body-3d+set-physics-material-override :class 'static-body-3d :bind
  "set_physics_material_override" :hash 1784508650)
 :void (physics-material-override physics-material))

(defgmethod
 (static-body-3d+get-physics-material-override :class 'static-body-3d :bind
  "get_physics_material_override" :hash 2521850424)
 physics-material)