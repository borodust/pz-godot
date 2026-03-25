(common-lisp:in-package :%godot)


(defgproperty voxel-gidata+dynamic-range 'voxel-gidata :get
 'voxel-gidata+get-dynamic-range :set 'voxel-gidata+set-dynamic-range)

(defgproperty voxel-gidata+energy 'voxel-gidata :get 'voxel-gidata+get-energy
 :set 'voxel-gidata+set-energy)

(defgproperty voxel-gidata+bias 'voxel-gidata :get 'voxel-gidata+get-bias :set
 'voxel-gidata+set-bias)

(defgproperty voxel-gidata+normal-bias 'voxel-gidata :get
 'voxel-gidata+get-normal-bias :set 'voxel-gidata+set-normal-bias)

(defgproperty voxel-gidata+propagation 'voxel-gidata :get
 'voxel-gidata+get-propagation :set 'voxel-gidata+set-propagation)

(defgproperty voxel-gidata+use-two-bounces 'voxel-gidata :get
 'voxel-gidata+is-using-two-bounces :set 'voxel-gidata+set-use-two-bounces)

(defgproperty voxel-gidata+interior 'voxel-gidata :get
 'voxel-gidata+is-interior :set 'voxel-gidata+set-interior)