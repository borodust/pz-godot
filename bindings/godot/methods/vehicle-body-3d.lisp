(common-lisp:in-package :%godot)


(defgmethod
 (vehicle-body-3d+set-engine-force :class 'vehicle-body-3d :bind
  "set_engine_force" :hash 373806689)
 :void (engine-force float))

(defgmethod
 (vehicle-body-3d+get-engine-force :class 'vehicle-body-3d :bind
  "get_engine_force" :hash 1740695150)
 float)

(defgmethod
 (vehicle-body-3d+set-brake :class 'vehicle-body-3d :bind "set_brake" :hash
  373806689)
 :void (brake float))

(defgmethod
 (vehicle-body-3d+get-brake :class 'vehicle-body-3d :bind "get_brake" :hash
  1740695150)
 float)

(defgmethod
 (vehicle-body-3d+set-steering :class 'vehicle-body-3d :bind "set_steering"
  :hash 373806689)
 :void (steering float))

(defgmethod
 (vehicle-body-3d+get-steering :class 'vehicle-body-3d :bind "get_steering"
  :hash 1740695150)
 float)