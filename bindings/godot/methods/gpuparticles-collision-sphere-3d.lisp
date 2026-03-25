(common-lisp:in-package :%godot)


(defgmethod
 (gpuparticles-collision-sphere-3d+set-radius :class
  'gpuparticles-collision-sphere-3d :bind "set_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (gpuparticles-collision-sphere-3d+get-radius :class
  'gpuparticles-collision-sphere-3d :bind "get_radius" :hash 1740695150)
 float)