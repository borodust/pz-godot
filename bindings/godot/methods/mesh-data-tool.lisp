(common-lisp:in-package :%godot)


(defgmethod
 (mesh-data-tool+clear :class 'mesh-data-tool :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (mesh-data-tool+create-from-surface :class 'mesh-data-tool :bind
  "create_from_surface" :hash 2727020678)
 error (mesh array-mesh) (surface int))

(defgmethod
 (mesh-data-tool+commit-to-surface :class 'mesh-data-tool :bind
  "commit_to_surface" :hash 2021686445)
 error (mesh array-mesh) (compression-flags int))

(defgmethod
 (mesh-data-tool+get-format :class 'mesh-data-tool :bind "get_format" :hash
  3905245786)
 int)

(defgmethod
 (mesh-data-tool+get-vertex-count :class 'mesh-data-tool :bind
  "get_vertex_count" :hash 3905245786)
 int)

(defgmethod
 (mesh-data-tool+get-edge-count :class 'mesh-data-tool :bind "get_edge_count"
  :hash 3905245786)
 int)

(defgmethod
 (mesh-data-tool+get-face-count :class 'mesh-data-tool :bind "get_face_count"
  :hash 3905245786)
 int)

(defgmethod
 (mesh-data-tool+set-vertex :class 'mesh-data-tool :bind "set_vertex" :hash
  1530502735)
 :void (idx int) (vertex vector-3))

(defgmethod
 (mesh-data-tool+get-vertex :class 'mesh-data-tool :bind "get_vertex" :hash
  711720468)
 vector-3 (idx int))

(defgmethod
 (mesh-data-tool+set-vertex-normal :class 'mesh-data-tool :bind
  "set_vertex_normal" :hash 1530502735)
 :void (idx int) (normal vector-3))

(defgmethod
 (mesh-data-tool+get-vertex-normal :class 'mesh-data-tool :bind
  "get_vertex_normal" :hash 711720468)
 vector-3 (idx int))

(defgmethod
 (mesh-data-tool+set-vertex-tangent :class 'mesh-data-tool :bind
  "set_vertex_tangent" :hash 1104099133)
 :void (idx int) (tangent plane))

(defgmethod
 (mesh-data-tool+get-vertex-tangent :class 'mesh-data-tool :bind
  "get_vertex_tangent" :hash 1372055458)
 plane (idx int))

(defgmethod
 (mesh-data-tool+set-vertex-uv :class 'mesh-data-tool :bind "set_vertex_uv"
  :hash 163021252)
 :void (idx int) (uv vector-2))

(defgmethod
 (mesh-data-tool+get-vertex-uv :class 'mesh-data-tool :bind "get_vertex_uv"
  :hash 2299179447)
 vector-2 (idx int))

(defgmethod
 (mesh-data-tool+set-vertex-uv2 :class 'mesh-data-tool :bind "set_vertex_uv2"
  :hash 163021252)
 :void (idx int) (uv2 vector-2))

(defgmethod
 (mesh-data-tool+get-vertex-uv2 :class 'mesh-data-tool :bind "get_vertex_uv2"
  :hash 2299179447)
 vector-2 (idx int))

(defgmethod
 (mesh-data-tool+set-vertex-color :class 'mesh-data-tool :bind
  "set_vertex_color" :hash 2878471219)
 :void (idx int) (color color))

(defgmethod
 (mesh-data-tool+get-vertex-color :class 'mesh-data-tool :bind
  "get_vertex_color" :hash 3457211756)
 color (idx int))

(defgmethod
 (mesh-data-tool+set-vertex-bones :class 'mesh-data-tool :bind
  "set_vertex_bones" :hash 3500328261)
 :void (idx int) (bones packed-int-32array))

(defgmethod
 (mesh-data-tool+get-vertex-bones :class 'mesh-data-tool :bind
  "get_vertex_bones" :hash 1706082319)
 packed-int-32array (idx int))

(defgmethod
 (mesh-data-tool+set-vertex-weights :class 'mesh-data-tool :bind
  "set_vertex_weights" :hash 1345852415)
 :void (idx int) (weights packed-float-32array))

(defgmethod
 (mesh-data-tool+get-vertex-weights :class 'mesh-data-tool :bind
  "get_vertex_weights" :hash 1542882410)
 packed-float-32array (idx int))

(defgmethod
 (mesh-data-tool+set-vertex-meta :class 'mesh-data-tool :bind "set_vertex_meta"
  :hash 2152698145)
 :void (idx int) (meta variant))

(defgmethod
 (mesh-data-tool+get-vertex-meta :class 'mesh-data-tool :bind "get_vertex_meta"
  :hash 4227898402)
 variant (idx int))

(defgmethod
 (mesh-data-tool+get-vertex-edges :class 'mesh-data-tool :bind
  "get_vertex_edges" :hash 1706082319)
 packed-int-32array (idx int))

(defgmethod
 (mesh-data-tool+get-vertex-faces :class 'mesh-data-tool :bind
  "get_vertex_faces" :hash 1706082319)
 packed-int-32array (idx int))

(defgmethod
 (mesh-data-tool+get-edge-vertex :class 'mesh-data-tool :bind "get_edge_vertex"
  :hash 3175239445)
 int (idx int) (vertex int))

(defgmethod
 (mesh-data-tool+get-edge-faces :class 'mesh-data-tool :bind "get_edge_faces"
  :hash 1706082319)
 packed-int-32array (idx int))

(defgmethod
 (mesh-data-tool+set-edge-meta :class 'mesh-data-tool :bind "set_edge_meta"
  :hash 2152698145)
 :void (idx int) (meta variant))

(defgmethod
 (mesh-data-tool+get-edge-meta :class 'mesh-data-tool :bind "get_edge_meta"
  :hash 4227898402)
 variant (idx int))

(defgmethod
 (mesh-data-tool+get-face-vertex :class 'mesh-data-tool :bind "get_face_vertex"
  :hash 3175239445)
 int (idx int) (vertex int))

(defgmethod
 (mesh-data-tool+get-face-edge :class 'mesh-data-tool :bind "get_face_edge"
  :hash 3175239445)
 int (idx int) (edge int))

(defgmethod
 (mesh-data-tool+set-face-meta :class 'mesh-data-tool :bind "set_face_meta"
  :hash 2152698145)
 :void (idx int) (meta variant))

(defgmethod
 (mesh-data-tool+get-face-meta :class 'mesh-data-tool :bind "get_face_meta"
  :hash 4227898402)
 variant (idx int))

(defgmethod
 (mesh-data-tool+get-face-normal :class 'mesh-data-tool :bind "get_face_normal"
  :hash 711720468)
 vector-3 (idx int))

(defgmethod
 (mesh-data-tool+set-material :class 'mesh-data-tool :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (mesh-data-tool+get-material :class 'mesh-data-tool :bind "get_material" :hash
  5934680)
 material)