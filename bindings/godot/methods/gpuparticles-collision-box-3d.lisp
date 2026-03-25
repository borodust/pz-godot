(common-lisp:in-package :%godot)


(defgmethod
 (gpuparticles-collision-box-3d+set-size :class 'gpuparticles-collision-box-3d
  :bind "set_size" :hash 3460891852)
 :void (size vector-3))

(defgmethod
 (gpuparticles-collision-box-3d+get-size :class 'gpuparticles-collision-box-3d
  :bind "get_size" :hash 3360562783)
 vector-3)