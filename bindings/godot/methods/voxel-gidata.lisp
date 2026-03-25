(common-lisp:in-package :%godot)


(defgmethod
 (voxel-gidata+allocate :class 'voxel-gidata :bind "allocate" :hash 4041601946)
 :void (to-cell-xform transform-3d) (aabb aabb) (octree-size vector-3)
 (octree-cells packed-byte-array) (data-cells packed-byte-array)
 (distance-field packed-byte-array) (level-counts packed-int-32array))

(defgmethod
 (voxel-gidata+get-bounds :class 'voxel-gidata :bind "get_bounds" :hash
  1068685055)
 aabb)

(defgmethod
 (voxel-gidata+get-octree-size :class 'voxel-gidata :bind "get_octree_size"
  :hash 3360562783)
 vector-3)

(defgmethod
 (voxel-gidata+get-to-cell-xform :class 'voxel-gidata :bind "get_to_cell_xform"
  :hash 3229777777)
 transform-3d)

(defgmethod
 (voxel-gidata+get-octree-cells :class 'voxel-gidata :bind "get_octree_cells"
  :hash 2362200018)
 packed-byte-array)

(defgmethod
 (voxel-gidata+get-data-cells :class 'voxel-gidata :bind "get_data_cells" :hash
  2362200018)
 packed-byte-array)

(defgmethod
 (voxel-gidata+get-level-counts :class 'voxel-gidata :bind "get_level_counts"
  :hash 1930428628)
 packed-int-32array)

(defgmethod
 (voxel-gidata+set-dynamic-range :class 'voxel-gidata :bind "set_dynamic_range"
  :hash 373806689)
 :void (dynamic-range float))

(defgmethod
 (voxel-gidata+get-dynamic-range :class 'voxel-gidata :bind "get_dynamic_range"
  :hash 1740695150)
 float)

(defgmethod
 (voxel-gidata+set-energy :class 'voxel-gidata :bind "set_energy" :hash
  373806689)
 :void (energy float))

(defgmethod
 (voxel-gidata+get-energy :class 'voxel-gidata :bind "get_energy" :hash
  1740695150)
 float)

(defgmethod
 (voxel-gidata+set-bias :class 'voxel-gidata :bind "set_bias" :hash 373806689)
 :void (bias float))

(defgmethod
 (voxel-gidata+get-bias :class 'voxel-gidata :bind "get_bias" :hash 1740695150)
 float)

(defgmethod
 (voxel-gidata+set-normal-bias :class 'voxel-gidata :bind "set_normal_bias"
  :hash 373806689)
 :void (bias float))

(defgmethod
 (voxel-gidata+get-normal-bias :class 'voxel-gidata :bind "get_normal_bias"
  :hash 1740695150)
 float)

(defgmethod
 (voxel-gidata+set-propagation :class 'voxel-gidata :bind "set_propagation"
  :hash 373806689)
 :void (propagation float))

(defgmethod
 (voxel-gidata+get-propagation :class 'voxel-gidata :bind "get_propagation"
  :hash 1740695150)
 float)

(defgmethod
 (voxel-gidata+set-interior :class 'voxel-gidata :bind "set_interior" :hash
  2586408642)
 :void (interior bool))

(defgmethod
 (voxel-gidata+is-interior :class 'voxel-gidata :bind "is_interior" :hash
  36873697)
 bool)

(defgmethod
 (voxel-gidata+set-use-two-bounces :class 'voxel-gidata :bind
  "set_use_two_bounces" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (voxel-gidata+is-using-two-bounces :class 'voxel-gidata :bind
  "is_using_two_bounces" :hash 36873697)
 bool)