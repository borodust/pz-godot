(common-lisp:in-package :%godot)


(defgmethod
 (navigation-mesh-source-geometry-data-3d+set-vertices :class
  'navigation-mesh-source-geometry-data-3d :bind "set_vertices" :hash
  2899603908)
 :void (vertices packed-float-32array))

(defgmethod
 (navigation-mesh-source-geometry-data-3d+get-vertices :class
  'navigation-mesh-source-geometry-data-3d :bind "get_vertices" :hash
  675695659)
 packed-float-32array)

(defgmethod
 (navigation-mesh-source-geometry-data-3d+set-indices :class
  'navigation-mesh-source-geometry-data-3d :bind "set_indices" :hash
  3614634198)
 :void (indices packed-int-32array))

(defgmethod
 (navigation-mesh-source-geometry-data-3d+get-indices :class
  'navigation-mesh-source-geometry-data-3d :bind "get_indices" :hash
  1930428628)
 packed-int-32array)

(defgmethod
 (navigation-mesh-source-geometry-data-3d+append-arrays :class
  'navigation-mesh-source-geometry-data-3d :bind "append_arrays" :hash
  3117535015)
 :void (vertices packed-float-32array) (indices packed-int-32array))

(defgmethod
 (navigation-mesh-source-geometry-data-3d+clear :class
  'navigation-mesh-source-geometry-data-3d :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (navigation-mesh-source-geometry-data-3d+has-data :class
  'navigation-mesh-source-geometry-data-3d :bind "has_data" :hash 2240911060)
 bool)

(defgmethod
 (navigation-mesh-source-geometry-data-3d+add-mesh :class
  'navigation-mesh-source-geometry-data-3d :bind "add_mesh" :hash 975462459)
 :void (mesh mesh) (xform transform-3d))

(defgmethod
 (navigation-mesh-source-geometry-data-3d+add-mesh-array :class
  'navigation-mesh-source-geometry-data-3d :bind "add_mesh_array" :hash
  4235710913)
 :void (mesh-array array) (xform transform-3d))

(defgmethod
 (navigation-mesh-source-geometry-data-3d+add-faces :class
  'navigation-mesh-source-geometry-data-3d :bind "add_faces" :hash 1440358797)
 :void (faces packed-vector-3array) (xform transform-3d))

(defgmethod
 (navigation-mesh-source-geometry-data-3d+merge :class
  'navigation-mesh-source-geometry-data-3d :bind "merge" :hash 655828145)
 :void (other-geometry navigation-mesh-source-geometry-data-3d))

(defgmethod
 (navigation-mesh-source-geometry-data-3d+add-projected-obstruction :class
  'navigation-mesh-source-geometry-data-3d :bind "add_projected_obstruction"
  :hash 3351846707)
 :void (vertices packed-vector-3array) (elevation float) (height float)
 (carve bool))

(defgmethod
 (navigation-mesh-source-geometry-data-3d+clear-projected-obstructions :class
  'navigation-mesh-source-geometry-data-3d :bind "clear_projected_obstructions"
  :hash 3218959716)
 :void)

(defgmethod
 (navigation-mesh-source-geometry-data-3d+set-projected-obstructions :class
  'navigation-mesh-source-geometry-data-3d :bind "set_projected_obstructions"
  :hash 381264803)
 :void (projected-obstructions array))

(defgmethod
 (navigation-mesh-source-geometry-data-3d+get-projected-obstructions :class
  'navigation-mesh-source-geometry-data-3d :bind "get_projected_obstructions"
  :hash 3995934104)
 array)

(defgmethod
 (navigation-mesh-source-geometry-data-3d+get-bounds :class
  'navigation-mesh-source-geometry-data-3d :bind "get_bounds" :hash 1021181044)
 aabb)