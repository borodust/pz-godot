(common-lisp:in-package :%godot)


(defgmethod
 (mesh+%get-surface-count :class 'mesh :bind "_get_surface_count" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (mesh+%surface-get-array-len :class 'mesh :bind "_surface_get_array_len" :hash
  923996154 :virtual common-lisp:t)
 int (index int))

(defgmethod
 (mesh+%surface-get-array-index-len :class 'mesh :bind
  "_surface_get_array_index_len" :hash 923996154 :virtual common-lisp:t)
 int (index int))

(defgmethod
 (mesh+%surface-get-arrays :class 'mesh :bind "_surface_get_arrays" :hash
  663333327 :virtual common-lisp:t)
 array (index int))

(defgmethod
 (mesh+%surface-get-blend-shape-arrays :class 'mesh :bind
  "_surface_get_blend_shape_arrays" :hash 663333327 :virtual common-lisp:t)
 array (index int))

(defgmethod
 (mesh+%surface-get-lods :class 'mesh :bind "_surface_get_lods" :hash
  3485342025 :virtual common-lisp:t)
 dictionary (index int))

(defgmethod
 (mesh+%surface-get-format :class 'mesh :bind "_surface_get_format" :hash
  923996154 :virtual common-lisp:t)
 int (index int))

(defgmethod
 (mesh+%surface-get-primitive-type :class 'mesh :bind
  "_surface_get_primitive_type" :hash 923996154 :virtual common-lisp:t)
 int (index int))

(defgmethod
 (mesh+%surface-set-material :class 'mesh :bind "_surface_set_material" :hash
  3671737478 :virtual common-lisp:t)
 :void (index int) (material material))

(defgmethod
 (mesh+%surface-get-material :class 'mesh :bind "_surface_get_material" :hash
  2897466400 :virtual common-lisp:t)
 material (index int))

(defgmethod
 (mesh+%get-blend-shape-count :class 'mesh :bind "_get_blend_shape_count" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (mesh+%get-blend-shape-name :class 'mesh :bind "_get_blend_shape_name" :hash
  659327637 :virtual common-lisp:t)
 string-name (index int))

(defgmethod
 (mesh+%set-blend-shape-name :class 'mesh :bind "_set_blend_shape_name" :hash
  3780747571 :virtual common-lisp:t)
 :void (index int) (name string-name))

(defgmethod
 (mesh+%get-aabb :class 'mesh :bind "_get_aabb" :hash 1068685055 :virtual
  common-lisp:t)
 aabb)

(defgmethod
 (mesh+set-lightmap-size-hint :class 'mesh :bind "set_lightmap_size_hint" :hash
  1130785943)
 :void (size vector-2i))

(defgmethod
 (mesh+get-lightmap-size-hint :class 'mesh :bind "get_lightmap_size_hint" :hash
  3690982128)
 vector-2i)

(defgmethod (mesh+get-aabb :class 'mesh :bind "get_aabb" :hash 1068685055) aabb)

(defgmethod (mesh+get-faces :class 'mesh :bind "get_faces" :hash 497664490)
 packed-vector-3array)

(defgmethod
 (mesh+get-surface-count :class 'mesh :bind "get_surface_count" :hash
  3905245786)
 int)

(defgmethod
 (mesh+surface-get-arrays :class 'mesh :bind "surface_get_arrays" :hash
  663333327)
 array (surf-idx int))

(defgmethod
 (mesh+surface-get-blend-shape-arrays :class 'mesh :bind
  "surface_get_blend_shape_arrays" :hash 663333327)
 array (surf-idx int))

(defgmethod
 (mesh+surface-set-material :class 'mesh :bind "surface_set_material" :hash
  3671737478)
 :void (surf-idx int) (material material))

(defgmethod
 (mesh+surface-get-material :class 'mesh :bind "surface_get_material" :hash
  2897466400)
 material (surf-idx int))

(defgmethod
 (mesh+create-placeholder :class 'mesh :bind "create_placeholder" :hash
  121922552)
 resource)

(defgmethod
 (mesh+create-trimesh-shape :class 'mesh :bind "create_trimesh_shape" :hash
  4160111210)
 concave-polygon-shape-3d)

(defgmethod
 (mesh+create-convex-shape :class 'mesh :bind "create_convex_shape" :hash
  2529984628)
 convex-polygon-shape-3d (clean bool) (simplify bool))

(defgmethod
 (mesh+create-outline :class 'mesh :bind "create_outline" :hash 1208642001)
 mesh (margin float))

(defgmethod
 (mesh+generate-triangle-mesh :class 'mesh :bind "generate_triangle_mesh" :hash
  3476533166)
 triangle-mesh)