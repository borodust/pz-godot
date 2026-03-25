(common-lisp:in-package :%godot)


(defgmethod
 (polygon-2d+set-polygon :class 'polygon-2d :bind "set_polygon" :hash
  1509147220)
 :void (polygon packed-vector-2array))

(defgmethod
 (polygon-2d+get-polygon :class 'polygon-2d :bind "get_polygon" :hash
  2961356807)
 packed-vector-2array)

(defgmethod
 (polygon-2d+set-uv :class 'polygon-2d :bind "set_uv" :hash 1509147220) :void
 (uv packed-vector-2array))

(defgmethod
 (polygon-2d+get-uv :class 'polygon-2d :bind "get_uv" :hash 2961356807)
 packed-vector-2array)

(defgmethod
 (polygon-2d+set-color :class 'polygon-2d :bind "set_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (polygon-2d+get-color :class 'polygon-2d :bind "get_color" :hash 3444240500)
 color)

(defgmethod
 (polygon-2d+set-polygons :class 'polygon-2d :bind "set_polygons" :hash
  381264803)
 :void (polygons array))

(defgmethod
 (polygon-2d+get-polygons :class 'polygon-2d :bind "get_polygons" :hash
  3995934104)
 array)

(defgmethod
 (polygon-2d+set-vertex-colors :class 'polygon-2d :bind "set_vertex_colors"
  :hash 3546319833)
 :void (vertex-colors packed-color-array))

(defgmethod
 (polygon-2d+get-vertex-colors :class 'polygon-2d :bind "get_vertex_colors"
  :hash 1392750486)
 packed-color-array)

(defgmethod
 (polygon-2d+set-texture :class 'polygon-2d :bind "set_texture" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (polygon-2d+get-texture :class 'polygon-2d :bind "get_texture" :hash
  3635182373)
 texture-2d)

(defgmethod
 (polygon-2d+set-texture-offset :class 'polygon-2d :bind "set_texture_offset"
  :hash 743155724)
 :void (texture-offset vector-2))

(defgmethod
 (polygon-2d+get-texture-offset :class 'polygon-2d :bind "get_texture_offset"
  :hash 3341600327)
 vector-2)

(defgmethod
 (polygon-2d+set-texture-rotation :class 'polygon-2d :bind
  "set_texture_rotation" :hash 373806689)
 :void (texture-rotation float))

(defgmethod
 (polygon-2d+get-texture-rotation :class 'polygon-2d :bind
  "get_texture_rotation" :hash 1740695150)
 float)

(defgmethod
 (polygon-2d+set-texture-scale :class 'polygon-2d :bind "set_texture_scale"
  :hash 743155724)
 :void (texture-scale vector-2))

(defgmethod
 (polygon-2d+get-texture-scale :class 'polygon-2d :bind "get_texture_scale"
  :hash 3341600327)
 vector-2)

(defgmethod
 (polygon-2d+set-invert-enabled :class 'polygon-2d :bind "set_invert_enabled"
  :hash 2586408642)
 :void (invert bool))

(defgmethod
 (polygon-2d+get-invert-enabled :class 'polygon-2d :bind "get_invert_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (polygon-2d+set-antialiased :class 'polygon-2d :bind "set_antialiased" :hash
  2586408642)
 :void (antialiased bool))

(defgmethod
 (polygon-2d+get-antialiased :class 'polygon-2d :bind "get_antialiased" :hash
  36873697)
 bool)

(defgmethod
 (polygon-2d+set-invert-border :class 'polygon-2d :bind "set_invert_border"
  :hash 373806689)
 :void (invert-border float))

(defgmethod
 (polygon-2d+get-invert-border :class 'polygon-2d :bind "get_invert_border"
  :hash 1740695150)
 float)

(defgmethod
 (polygon-2d+set-offset :class 'polygon-2d :bind "set_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (polygon-2d+get-offset :class 'polygon-2d :bind "get_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (polygon-2d+add-bone :class 'polygon-2d :bind "add_bone" :hash 703042815)
 :void (path node-path) (weights packed-float-32array))

(defgmethod
 (polygon-2d+get-bone-count :class 'polygon-2d :bind "get_bone_count" :hash
  3905245786)
 int)

(defgmethod
 (polygon-2d+get-bone-path :class 'polygon-2d :bind "get_bone_path" :hash
  408788394)
 node-path (index int))

(defgmethod
 (polygon-2d+get-bone-weights :class 'polygon-2d :bind "get_bone_weights" :hash
  1542882410)
 packed-float-32array (index int))

(defgmethod
 (polygon-2d+erase-bone :class 'polygon-2d :bind "erase_bone" :hash 1286410249)
 :void (index int))

(defgmethod
 (polygon-2d+clear-bones :class 'polygon-2d :bind "clear_bones" :hash
  3218959716)
 :void)

(defgmethod
 (polygon-2d+set-bone-path :class 'polygon-2d :bind "set_bone_path" :hash
  2761262315)
 :void (index int) (path node-path))

(defgmethod
 (polygon-2d+set-bone-weights :class 'polygon-2d :bind "set_bone_weights" :hash
  1345852415)
 :void (index int) (weights packed-float-32array))

(defgmethod
 (polygon-2d+set-skeleton :class 'polygon-2d :bind "set_skeleton" :hash
  1348162250)
 :void (skeleton node-path))

(defgmethod
 (polygon-2d+get-skeleton :class 'polygon-2d :bind "get_skeleton" :hash
  4075236667)
 node-path)

(defgmethod
 (polygon-2d+set-internal-vertex-count :class 'polygon-2d :bind
  "set_internal_vertex_count" :hash 1286410249)
 :void (internal-vertex-count int))

(defgmethod
 (polygon-2d+get-internal-vertex-count :class 'polygon-2d :bind
  "get_internal_vertex_count" :hash 3905245786)
 int)