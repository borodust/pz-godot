(common-lisp:in-package :%godot)


(defgmethod
 (voxel-gi+set-probe-data :class 'voxel-gi :bind "set_probe_data" :hash
  1637849675)
 :void (data voxel-gidata))

(defgmethod
 (voxel-gi+get-probe-data :class 'voxel-gi :bind "get_probe_data" :hash
  1730645405)
 voxel-gidata)

(defgmethod
 (voxel-gi+set-subdiv :class 'voxel-gi :bind "set_subdiv" :hash 2240898472)
 :void (subdiv voxel-gi+subdiv))

(defgmethod
 (voxel-gi+get-subdiv :class 'voxel-gi :bind "get_subdiv" :hash 4261647950)
 voxel-gi+subdiv)

(defgmethod
 (voxel-gi+set-size :class 'voxel-gi :bind "set_size" :hash 3460891852) :void
 (size vector-3))

(defgmethod
 (voxel-gi+get-size :class 'voxel-gi :bind "get_size" :hash 3360562783)
 vector-3)

(defgmethod
 (voxel-gi+set-camera-attributes :class 'voxel-gi :bind "set_camera_attributes"
  :hash 2817810567)
 :void (camera-attributes camera-attributes))

(defgmethod
 (voxel-gi+get-camera-attributes :class 'voxel-gi :bind "get_camera_attributes"
  :hash 3921283215)
 camera-attributes)

(defgmethod (voxel-gi+bake :class 'voxel-gi :bind "bake" :hash 2781551026)
 :void (from-node node) (create-visual-debug bool))

(defgmethod
 (voxel-gi+debug-bake :class 'voxel-gi :bind "debug_bake" :hash 3218959716)
 :void)