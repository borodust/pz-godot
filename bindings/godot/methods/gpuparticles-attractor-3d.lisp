(common-lisp:in-package :%godot)


(defgmethod
 (gpuparticles-attractor-3d+set-cull-mask :class 'gpuparticles-attractor-3d
  :bind "set_cull_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (gpuparticles-attractor-3d+get-cull-mask :class 'gpuparticles-attractor-3d
  :bind "get_cull_mask" :hash 3905245786)
 int)

(defgmethod
 (gpuparticles-attractor-3d+set-strength :class 'gpuparticles-attractor-3d
  :bind "set_strength" :hash 373806689)
 :void (strength float))

(defgmethod
 (gpuparticles-attractor-3d+get-strength :class 'gpuparticles-attractor-3d
  :bind "get_strength" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-attractor-3d+set-attenuation :class 'gpuparticles-attractor-3d
  :bind "set_attenuation" :hash 373806689)
 :void (attenuation float))

(defgmethod
 (gpuparticles-attractor-3d+get-attenuation :class 'gpuparticles-attractor-3d
  :bind "get_attenuation" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-attractor-3d+set-directionality :class
  'gpuparticles-attractor-3d :bind "set_directionality" :hash 373806689)
 :void (amount float))

(defgmethod
 (gpuparticles-attractor-3d+get-directionality :class
  'gpuparticles-attractor-3d :bind "get_directionality" :hash 1740695150)
 float)