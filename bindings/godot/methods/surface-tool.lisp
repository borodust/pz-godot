(common-lisp:in-package :%godot)


(defgmethod
 (surface-tool+set-skin-weight-count :class 'surface-tool :bind
  "set_skin_weight_count" :hash 618679515)
 :void (count surface-tool+skin-weight-count))

(defgmethod
 (surface-tool+get-skin-weight-count :class 'surface-tool :bind
  "get_skin_weight_count" :hash 1072401130)
 surface-tool+skin-weight-count)

(defgmethod
 (surface-tool+set-custom-format :class 'surface-tool :bind "set_custom_format"
  :hash 4087759856)
 :void (channel-index int) (format surface-tool+custom-format))

(defgmethod
 (surface-tool+get-custom-format :class 'surface-tool :bind "get_custom_format"
  :hash 839863283)
 surface-tool+custom-format (channel-index int))

(defgmethod
 (surface-tool+begin :class 'surface-tool :bind "begin" :hash 2230304113) :void
 (primitive mesh+primitive-type))

(defgmethod
 (surface-tool+add-vertex :class 'surface-tool :bind "add_vertex" :hash
  3460891852)
 :void (vertex vector-3))

(defgmethod
 (surface-tool+set-color :class 'surface-tool :bind "set_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (surface-tool+set-normal :class 'surface-tool :bind "set_normal" :hash
  3460891852)
 :void (normal vector-3))

(defgmethod
 (surface-tool+set-tangent :class 'surface-tool :bind "set_tangent" :hash
  3505987427)
 :void (tangent plane))

(defgmethod
 (surface-tool+set-uv :class 'surface-tool :bind "set_uv" :hash 743155724)
 :void (uv vector-2))

(defgmethod
 (surface-tool+set-uv2 :class 'surface-tool :bind "set_uv2" :hash 743155724)
 :void (uv2 vector-2))

(defgmethod
 (surface-tool+set-bones :class 'surface-tool :bind "set_bones" :hash
  3614634198)
 :void (bones packed-int-32array))

(defgmethod
 (surface-tool+set-weights :class 'surface-tool :bind "set_weights" :hash
  2899603908)
 :void (weights packed-float-32array))

(defgmethod
 (surface-tool+set-custom :class 'surface-tool :bind "set_custom" :hash
  2878471219)
 :void (channel-index int) (custom-color color))

(defgmethod
 (surface-tool+set-smooth-group :class 'surface-tool :bind "set_smooth_group"
  :hash 1286410249)
 :void (index int))

(defgmethod
 (surface-tool+add-triangle-fan :class 'surface-tool :bind "add_triangle_fan"
  :hash 2235017613)
 :void (vertices packed-vector-3array) (uvs packed-vector-2array)
 (colors packed-color-array) (uv2s packed-vector-2array)
 (normals packed-vector-3array) (tangents array))

(defgmethod
 (surface-tool+add-index :class 'surface-tool :bind "add_index" :hash
  1286410249)
 :void (index int))

(defgmethod
 (surface-tool+index :class 'surface-tool :bind "index" :hash 3218959716) :void)

(defgmethod
 (surface-tool+deindex :class 'surface-tool :bind "deindex" :hash 3218959716)
 :void)

(defgmethod
 (surface-tool+generate-normals :class 'surface-tool :bind "generate_normals"
  :hash 107499316)
 :void (flip bool))

(defgmethod
 (surface-tool+generate-tangents :class 'surface-tool :bind "generate_tangents"
  :hash 3218959716)
 :void)

(defgmethod
 (surface-tool+optimize-indices-for-cache :class 'surface-tool :bind
  "optimize_indices_for_cache" :hash 3218959716)
 :void)

(defgmethod
 (surface-tool+get-aabb :class 'surface-tool :bind "get_aabb" :hash 1068685055)
 aabb)

(defgmethod
 (surface-tool+generate-lod :class 'surface-tool :bind "generate_lod" :hash
  1938056459)
 packed-int-32array (nd-threshold float) (target-index-count int))

(defgmethod
 (surface-tool+set-material :class 'surface-tool :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (surface-tool+get-primitive-type :class 'surface-tool :bind
  "get_primitive_type" :hash 768822145)
 mesh+primitive-type)

(defgmethod
 (surface-tool+clear :class 'surface-tool :bind "clear" :hash 3218959716) :void)

(defgmethod
 (surface-tool+create-from :class 'surface-tool :bind "create_from" :hash
  1767024570)
 :void (existing mesh) (surface int))

(defgmethod
 (surface-tool+create-from-arrays :class 'surface-tool :bind
  "create_from_arrays" :hash 1894639680)
 :void (arrays array) (primitive-type mesh+primitive-type))

(defgmethod
 (surface-tool+create-from-blend-shape :class 'surface-tool :bind
  "create_from_blend_shape" :hash 1306185582)
 :void (existing mesh) (surface int) (blend-shape string))

(defgmethod
 (surface-tool+append-from :class 'surface-tool :bind "append_from" :hash
  2217967155)
 :void (existing mesh) (surface int) (transform transform-3d))

(defgmethod
 (surface-tool+commit :class 'surface-tool :bind "commit" :hash 4107864055)
 array-mesh (existing array-mesh) (flags int))

(defgmethod
 (surface-tool+commit-to-arrays :class 'surface-tool :bind "commit_to_arrays"
  :hash 2915620761)
 array)