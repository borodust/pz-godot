(common-lisp:in-package :%godot)


(defgmethod
 (open-xrplane-tracker+set-bounds-size :class 'open-xrplane-tracker :bind
  "set_bounds_size" :hash 743155724)
 :void (bounds-size vector-2))

(defgmethod
 (open-xrplane-tracker+get-bounds-size :class 'open-xrplane-tracker :bind
  "get_bounds_size" :hash 3341600327)
 vector-2)

(defgmethod
 (open-xrplane-tracker+set-plane-alignment :class 'open-xrplane-tracker :bind
  "set_plane_alignment" :hash 1214382230)
 :void
 (plane-alignment
  open-xrspatial-component-plane-alignment-list+plane-alignment))

(defgmethod
 (open-xrplane-tracker+get-plane-alignment :class 'open-xrplane-tracker :bind
  "get_plane_alignment" :hash 845541441)
 open-xrspatial-component-plane-alignment-list+plane-alignment)

(defgmethod
 (open-xrplane-tracker+set-plane-label :class 'open-xrplane-tracker :bind
  "set_plane_label" :hash 83702148)
 :void (plane-label string))

(defgmethod
 (open-xrplane-tracker+get-plane-label :class 'open-xrplane-tracker :bind
  "get_plane_label" :hash 201670096)
 string)

(defgmethod
 (open-xrplane-tracker+set-mesh-data :class 'open-xrplane-tracker :bind
  "set_mesh_data" :hash 1877193149)
 :void (origin transform-3d) (vertices packed-vector-2array)
 (indices packed-int-32array))

(defgmethod
 (open-xrplane-tracker+clear-mesh-data :class 'open-xrplane-tracker :bind
  "clear_mesh_data" :hash 3218959716)
 :void)

(defgmethod
 (open-xrplane-tracker+get-mesh-offset :class 'open-xrplane-tracker :bind
  "get_mesh_offset" :hash 3229777777)
 transform-3d)

(defgmethod
 (open-xrplane-tracker+get-mesh :class 'open-xrplane-tracker :bind "get_mesh"
  :hash 4081188045)
 mesh)

(defgmethod
 (open-xrplane-tracker+get-shape :class 'open-xrplane-tracker :bind "get_shape"
  :hash 3358509884)
 shape-3d (thickness float))