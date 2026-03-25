(common-lisp:in-package :%godot)


(defgmethod
 (gpuparticles-collision-height-field-3d+set-size :class
  'gpuparticles-collision-height-field-3d :bind "set_size" :hash 3460891852)
 :void (size vector-3))

(defgmethod
 (gpuparticles-collision-height-field-3d+get-size :class
  'gpuparticles-collision-height-field-3d :bind "get_size" :hash 3360562783)
 vector-3)

(defgmethod
 (gpuparticles-collision-height-field-3d+set-resolution :class
  'gpuparticles-collision-height-field-3d :bind "set_resolution" :hash
  1009996517)
 :void (resolution gpuparticles-collision-height-field-3d+resolution))

(defgmethod
 (gpuparticles-collision-height-field-3d+get-resolution :class
  'gpuparticles-collision-height-field-3d :bind "get_resolution" :hash
  1156065644)
 gpuparticles-collision-height-field-3d+resolution)

(defgmethod
 (gpuparticles-collision-height-field-3d+set-update-mode :class
  'gpuparticles-collision-height-field-3d :bind "set_update_mode" :hash
  673680859)
 :void (update-mode gpuparticles-collision-height-field-3d+update-mode))

(defgmethod
 (gpuparticles-collision-height-field-3d+get-update-mode :class
  'gpuparticles-collision-height-field-3d :bind "get_update_mode" :hash
  1998141380)
 gpuparticles-collision-height-field-3d+update-mode)

(defgmethod
 (gpuparticles-collision-height-field-3d+set-heightfield-mask :class
  'gpuparticles-collision-height-field-3d :bind "set_heightfield_mask" :hash
  1286410249)
 :void (heightfield-mask int))

(defgmethod
 (gpuparticles-collision-height-field-3d+get-heightfield-mask :class
  'gpuparticles-collision-height-field-3d :bind "get_heightfield_mask" :hash
  3905245786)
 int)

(defgmethod
 (gpuparticles-collision-height-field-3d+set-heightfield-mask-value :class
  'gpuparticles-collision-height-field-3d :bind "set_heightfield_mask_value"
  :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (gpuparticles-collision-height-field-3d+get-heightfield-mask-value :class
  'gpuparticles-collision-height-field-3d :bind "get_heightfield_mask_value"
  :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (gpuparticles-collision-height-field-3d+set-follow-camera-enabled :class
  'gpuparticles-collision-height-field-3d :bind "set_follow_camera_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (gpuparticles-collision-height-field-3d+is-follow-camera-enabled :class
  'gpuparticles-collision-height-field-3d :bind "is_follow_camera_enabled"
  :hash 36873697)
 bool)