(common-lisp:in-package :%godot)


(defgmethod
 (gpuparticles-collision-sdf3d+set-size :class 'gpuparticles-collision-sdf3d
  :bind "set_size" :hash 3460891852)
 :void (size vector-3))

(defgmethod
 (gpuparticles-collision-sdf3d+get-size :class 'gpuparticles-collision-sdf3d
  :bind "get_size" :hash 3360562783)
 vector-3)

(defgmethod
 (gpuparticles-collision-sdf3d+set-resolution :class
  'gpuparticles-collision-sdf3d :bind "set_resolution" :hash 1155629297)
 :void (resolution gpuparticles-collision-sdf3d+resolution))

(defgmethod
 (gpuparticles-collision-sdf3d+get-resolution :class
  'gpuparticles-collision-sdf3d :bind "get_resolution" :hash 2919555867)
 gpuparticles-collision-sdf3d+resolution)

(defgmethod
 (gpuparticles-collision-sdf3d+set-texture :class 'gpuparticles-collision-sdf3d
  :bind "set_texture" :hash 1188404210)
 :void (texture texture-3d))

(defgmethod
 (gpuparticles-collision-sdf3d+get-texture :class 'gpuparticles-collision-sdf3d
  :bind "get_texture" :hash 373985333)
 texture-3d)

(defgmethod
 (gpuparticles-collision-sdf3d+set-thickness :class
  'gpuparticles-collision-sdf3d :bind "set_thickness" :hash 373806689)
 :void (thickness float))

(defgmethod
 (gpuparticles-collision-sdf3d+get-thickness :class
  'gpuparticles-collision-sdf3d :bind "get_thickness" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-collision-sdf3d+set-bake-mask :class
  'gpuparticles-collision-sdf3d :bind "set_bake_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (gpuparticles-collision-sdf3d+get-bake-mask :class
  'gpuparticles-collision-sdf3d :bind "get_bake_mask" :hash 3905245786)
 int)

(defgmethod
 (gpuparticles-collision-sdf3d+set-bake-mask-value :class
  'gpuparticles-collision-sdf3d :bind "set_bake_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (gpuparticles-collision-sdf3d+get-bake-mask-value :class
  'gpuparticles-collision-sdf3d :bind "get_bake_mask_value" :hash 1116898809)
 bool (layer-number int))