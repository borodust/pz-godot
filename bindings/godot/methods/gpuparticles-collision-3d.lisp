(common-lisp:in-package :%godot)


(defgmethod
 (gpuparticles-collision-3d+set-cull-mask :class 'gpuparticles-collision-3d
  :bind "set_cull_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (gpuparticles-collision-3d+get-cull-mask :class 'gpuparticles-collision-3d
  :bind "get_cull_mask" :hash 3905245786)
 int)