(common-lisp:in-package :%godot)


(defgmethod
 (gpuparticles-attractor-vector-field-3d+set-size :class
  'gpuparticles-attractor-vector-field-3d :bind "set_size" :hash 3460891852)
 :void (size vector-3))

(defgmethod
 (gpuparticles-attractor-vector-field-3d+get-size :class
  'gpuparticles-attractor-vector-field-3d :bind "get_size" :hash 3360562783)
 vector-3)

(defgmethod
 (gpuparticles-attractor-vector-field-3d+set-texture :class
  'gpuparticles-attractor-vector-field-3d :bind "set_texture" :hash 1188404210)
 :void (texture texture-3d))

(defgmethod
 (gpuparticles-attractor-vector-field-3d+get-texture :class
  'gpuparticles-attractor-vector-field-3d :bind "get_texture" :hash 373985333)
 texture-3d)