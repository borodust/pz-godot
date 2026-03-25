(common-lisp:in-package :%godot)


(defgmethod
 (array-mesh+add-blend-shape :class 'array-mesh :bind "add_blend_shape" :hash
  3304788590)
 :void (name string-name))

(defgmethod
 (array-mesh+get-blend-shape-count :class 'array-mesh :bind
  "get_blend_shape_count" :hash 3905245786)
 int)

(defgmethod
 (array-mesh+get-blend-shape-name :class 'array-mesh :bind
  "get_blend_shape_name" :hash 659327637)
 string-name (index int))

(defgmethod
 (array-mesh+set-blend-shape-name :class 'array-mesh :bind
  "set_blend_shape_name" :hash 3780747571)
 :void (index int) (name string-name))

(defgmethod
 (array-mesh+clear-blend-shapes :class 'array-mesh :bind "clear_blend_shapes"
  :hash 3218959716)
 :void)

(defgmethod
 (array-mesh+set-blend-shape-mode :class 'array-mesh :bind
  "set_blend_shape_mode" :hash 227983991)
 :void (mode mesh+blend-shape-mode))

(defgmethod
 (array-mesh+get-blend-shape-mode :class 'array-mesh :bind
  "get_blend_shape_mode" :hash 836485024)
 mesh+blend-shape-mode)

(defgmethod
 (array-mesh+add-surface-from-arrays :class 'array-mesh :bind
  "add_surface_from_arrays" :hash 1796411378)
 :void (primitive mesh+primitive-type) (arrays array) (blend-shapes array)
 (lods dictionary) (flags mesh+array-format))

(defgmethod
 (array-mesh+clear-surfaces :class 'array-mesh :bind "clear_surfaces" :hash
  3218959716)
 :void)

(defgmethod
 (array-mesh+surface-remove :class 'array-mesh :bind "surface_remove" :hash
  1286410249)
 :void (surf-idx int))

(defgmethod
 (array-mesh+surface-update-vertex-region :class 'array-mesh :bind
  "surface_update_vertex_region" :hash 3837166854)
 :void (surf-idx int) (offset int) (data packed-byte-array))

(defgmethod
 (array-mesh+surface-update-attribute-region :class 'array-mesh :bind
  "surface_update_attribute_region" :hash 3837166854)
 :void (surf-idx int) (offset int) (data packed-byte-array))

(defgmethod
 (array-mesh+surface-update-skin-region :class 'array-mesh :bind
  "surface_update_skin_region" :hash 3837166854)
 :void (surf-idx int) (offset int) (data packed-byte-array))

(defgmethod
 (array-mesh+surface-get-array-len :class 'array-mesh :bind
  "surface_get_array_len" :hash 923996154)
 int (surf-idx int))

(defgmethod
 (array-mesh+surface-get-array-index-len :class 'array-mesh :bind
  "surface_get_array_index_len" :hash 923996154)
 int (surf-idx int))

(defgmethod
 (array-mesh+surface-get-format :class 'array-mesh :bind "surface_get_format"
  :hash 3718287884)
 mesh+array-format (surf-idx int))

(defgmethod
 (array-mesh+surface-get-primitive-type :class 'array-mesh :bind
  "surface_get_primitive_type" :hash 4141943888)
 mesh+primitive-type (surf-idx int))

(defgmethod
 (array-mesh+surface-find-by-name :class 'array-mesh :bind
  "surface_find_by_name" :hash 1321353865)
 int (name string))

(defgmethod
 (array-mesh+surface-set-name :class 'array-mesh :bind "surface_set_name" :hash
  501894301)
 :void (surf-idx int) (name string))

(defgmethod
 (array-mesh+surface-get-name :class 'array-mesh :bind "surface_get_name" :hash
  844755477)
 string (surf-idx int))

(defgmethod
 (array-mesh+regen-normal-maps :class 'array-mesh :bind "regen_normal_maps"
  :hash 3218959716)
 :void)

(defgmethod
 (array-mesh+lightmap-unwrap :class 'array-mesh :bind "lightmap_unwrap" :hash
  1476641071)
 error (transform transform-3d) (texel-size float))

(defgmethod
 (array-mesh+set-custom-aabb :class 'array-mesh :bind "set_custom_aabb" :hash
  259215842)
 :void (aabb aabb))

(defgmethod
 (array-mesh+get-custom-aabb :class 'array-mesh :bind "get_custom_aabb" :hash
  1068685055)
 aabb)

(defgmethod
 (array-mesh+set-shadow-mesh :class 'array-mesh :bind "set_shadow_mesh" :hash
  3377897901)
 :void (mesh array-mesh))

(defgmethod
 (array-mesh+get-shadow-mesh :class 'array-mesh :bind "get_shadow_mesh" :hash
  3206942465)
 array-mesh)