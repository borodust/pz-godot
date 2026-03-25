(common-lisp:in-package :%godot)


(defgmethod
 (static-body-2d+set-constant-linear-velocity :class 'static-body-2d :bind
  "set_constant_linear_velocity" :hash 743155724)
 :void (vel vector-2))

(defgmethod
 (static-body-2d+set-constant-angular-velocity :class 'static-body-2d :bind
  "set_constant_angular_velocity" :hash 373806689)
 :void (vel float))

(defgmethod
 (static-body-2d+get-constant-linear-velocity :class 'static-body-2d :bind
  "get_constant_linear_velocity" :hash 3341600327)
 vector-2)

(defgmethod
 (static-body-2d+get-constant-angular-velocity :class 'static-body-2d :bind
  "get_constant_angular_velocity" :hash 1740695150)
 float)

(defgmethod
 (static-body-2d+set-physics-material-override :class 'static-body-2d :bind
  "set_physics_material_override" :hash 1784508650)
 :void (physics-material-override physics-material))

(defgmethod
 (static-body-2d+get-physics-material-override :class 'static-body-2d :bind
  "get_physics_material_override" :hash 2521850424)
 physics-material)