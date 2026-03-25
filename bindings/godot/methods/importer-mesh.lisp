(common-lisp:in-package :%godot)


(defgmethod
 (importer-mesh+add-blend-shape :class 'importer-mesh :bind "add_blend_shape"
  :hash 83702148)
 :void (name string))

(defgmethod
 (importer-mesh+get-blend-shape-count :class 'importer-mesh :bind
  "get_blend_shape_count" :hash 3905245786)
 int)

(defgmethod
 (importer-mesh+get-blend-shape-name :class 'importer-mesh :bind
  "get_blend_shape_name" :hash 844755477)
 string (blend-shape-idx int))

(defgmethod
 (importer-mesh+set-blend-shape-mode :class 'importer-mesh :bind
  "set_blend_shape_mode" :hash 227983991)
 :void (mode mesh+blend-shape-mode))

(defgmethod
 (importer-mesh+get-blend-shape-mode :class 'importer-mesh :bind
  "get_blend_shape_mode" :hash 836485024)
 mesh+blend-shape-mode)

(defgmethod
 (importer-mesh+add-surface :class 'importer-mesh :bind "add_surface" :hash
  1740448849)
 :void (primitive mesh+primitive-type) (arrays array) (blend-shapes array)
 (lods dictionary) (material material) (name string) (flags int))

(defgmethod
 (importer-mesh+get-surface-count :class 'importer-mesh :bind
  "get_surface_count" :hash 3905245786)
 int)

(defgmethod
 (importer-mesh+get-surface-primitive-type :class 'importer-mesh :bind
  "get_surface_primitive_type" :hash 3552571330)
 mesh+primitive-type (surface-idx int))

(defgmethod
 (importer-mesh+get-surface-name :class 'importer-mesh :bind "get_surface_name"
  :hash 844755477)
 string (surface-idx int))

(defgmethod
 (importer-mesh+get-surface-arrays :class 'importer-mesh :bind
  "get_surface_arrays" :hash 663333327)
 array (surface-idx int))

(defgmethod
 (importer-mesh+get-surface-blend-shape-arrays :class 'importer-mesh :bind
  "get_surface_blend_shape_arrays" :hash 2345056839)
 array (surface-idx int) (blend-shape-idx int))

(defgmethod
 (importer-mesh+get-surface-lod-count :class 'importer-mesh :bind
  "get_surface_lod_count" :hash 923996154)
 int (surface-idx int))

(defgmethod
 (importer-mesh+get-surface-lod-size :class 'importer-mesh :bind
  "get_surface_lod_size" :hash 3085491603)
 float (surface-idx int) (lod-idx int))

(defgmethod
 (importer-mesh+get-surface-lod-indices :class 'importer-mesh :bind
  "get_surface_lod_indices" :hash 1265128013)
 packed-int-32array (surface-idx int) (lod-idx int))

(defgmethod
 (importer-mesh+get-surface-material :class 'importer-mesh :bind
  "get_surface_material" :hash 2897466400)
 material (surface-idx int))

(defgmethod
 (importer-mesh+get-surface-format :class 'importer-mesh :bind
  "get_surface_format" :hash 923996154)
 int (surface-idx int))

(defgmethod
 (importer-mesh+set-surface-name :class 'importer-mesh :bind "set_surface_name"
  :hash 501894301)
 :void (surface-idx int) (name string))

(defgmethod
 (importer-mesh+set-surface-material :class 'importer-mesh :bind
  "set_surface_material" :hash 3671737478)
 :void (surface-idx int) (material material))

(defgmethod
 (importer-mesh+generate-lods :class 'importer-mesh :bind "generate_lods" :hash
  2491878677)
 :void (normal-merge-angle float) (normal-split-angle float)
 (bone-transform-array array))

(defgmethod
 (importer-mesh+get-mesh :class 'importer-mesh :bind "get_mesh" :hash
  1457573577)
 array-mesh (base-mesh array-mesh))

(defgmethod
 (importer-mesh+from-mesh :class 'importer-mesh :bind "from_mesh" :hash
  283226343 :static common-lisp:t)
 importer-mesh (mesh mesh))

(defgmethod
 (importer-mesh+clear :class 'importer-mesh :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (importer-mesh+set-lightmap-size-hint :class 'importer-mesh :bind
  "set_lightmap_size_hint" :hash 1130785943)
 :void (size vector-2i))

(defgmethod
 (importer-mesh+get-lightmap-size-hint :class 'importer-mesh :bind
  "get_lightmap_size_hint" :hash 3690982128)
 vector-2i)