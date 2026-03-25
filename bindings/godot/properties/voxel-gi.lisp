(common-lisp:in-package :%godot)


(defgproperty voxel-gi+subdiv 'voxel-gi :get 'voxel-gi+get-subdiv :set
 'voxel-gi+set-subdiv)

(defgproperty voxel-gi+size 'voxel-gi :get 'voxel-gi+get-size :set
 'voxel-gi+set-size)

(defgproperty voxel-gi+camera-attributes 'voxel-gi :get
 'voxel-gi+get-camera-attributes :set 'voxel-gi+set-camera-attributes)

(defgproperty voxel-gi+data 'voxel-gi :get 'voxel-gi+get-probe-data :set
 'voxel-gi+set-probe-data)