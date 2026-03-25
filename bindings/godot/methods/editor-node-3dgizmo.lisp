(common-lisp:in-package :%godot)


(defgmethod
 (editor-node-3dgizmo+-redraw :class 'editor-node-3dgizmo :bind "_redraw" :hash
  3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-node-3dgizmo+-get-handle-name :class 'editor-node-3dgizmo :bind
  "_get_handle_name" :hash 1868713439 :virtual common-lisp:t)
 string (id int) (secondary bool))

(defgmethod
 (editor-node-3dgizmo+-is-handle-highlighted :class 'editor-node-3dgizmo :bind
  "_is_handle_highlighted" :hash 361316320 :virtual common-lisp:t)
 bool (id int) (secondary bool))

(defgmethod
 (editor-node-3dgizmo+-get-handle-value :class 'editor-node-3dgizmo :bind
  "_get_handle_value" :hash 2144196525 :virtual common-lisp:t)
 variant (id int) (secondary bool))

(defgmethod
 (editor-node-3dgizmo+-begin-handle-action :class 'editor-node-3dgizmo :bind
  "_begin_handle_action" :hash 300928843 :virtual common-lisp:t)
 :void (id int) (secondary bool))

(defgmethod
 (editor-node-3dgizmo+-set-handle :class 'editor-node-3dgizmo :bind
  "_set_handle" :hash 2210262157 :virtual common-lisp:t)
 :void (id int) (secondary bool) (camera camera-3d) (point vector-2))

(defgmethod
 (editor-node-3dgizmo+-commit-handle :class 'editor-node-3dgizmo :bind
  "_commit_handle" :hash 3655739840 :virtual common-lisp:t)
 :void (id int) (secondary bool) (restore variant) (cancel bool))

(defgmethod
 (editor-node-3dgizmo+-subgizmos-intersect-ray :class 'editor-node-3dgizmo
  :bind "_subgizmos_intersect_ray" :hash 2055005479 :virtual common-lisp:t)
 int (camera camera-3d) (point vector-2))

(defgmethod
 (editor-node-3dgizmo+-subgizmos-intersect-frustum :class 'editor-node-3dgizmo
  :bind "_subgizmos_intersect_frustum" :hash 1653813165 :virtual common-lisp:t)
 packed-int-32array (camera camera-3d) (frustum array))

(defgmethod
 (editor-node-3dgizmo+-set-subgizmo-transform :class 'editor-node-3dgizmo :bind
  "_set_subgizmo_transform" :hash 3616898986 :virtual common-lisp:t)
 :void (id int) (transform transform-3d))

(defgmethod
 (editor-node-3dgizmo+-get-subgizmo-transform :class 'editor-node-3dgizmo :bind
  "_get_subgizmo_transform" :hash 1965739696 :virtual common-lisp:t)
 transform-3d (id int))

(defgmethod
 (editor-node-3dgizmo+-commit-subgizmos :class 'editor-node-3dgizmo :bind
  "_commit_subgizmos" :hash 3411059856 :virtual common-lisp:t)
 :void (ids packed-int-32array) (restores array) (cancel bool))

(defgmethod
 (editor-node-3dgizmo+add-lines :class 'editor-node-3dgizmo :bind "add_lines"
  :hash 2910971437)
 :void (lines packed-vector-3array) (material material) (billboard bool)
 (modulate color))

(defgmethod
 (editor-node-3dgizmo+add-mesh :class 'editor-node-3dgizmo :bind "add_mesh"
  :hash 1579955111)
 :void (mesh mesh) (material material) (transform transform-3d)
 (skeleton skin-reference))

(defgmethod
 (editor-node-3dgizmo+add-collision-segments :class 'editor-node-3dgizmo :bind
  "add_collision_segments" :hash 334873810)
 :void (segments packed-vector-3array))

(defgmethod
 (editor-node-3dgizmo+add-collision-triangles :class 'editor-node-3dgizmo :bind
  "add_collision_triangles" :hash 54901064)
 :void (triangles triangle-mesh))

(defgmethod
 (editor-node-3dgizmo+add-unscaled-billboard :class 'editor-node-3dgizmo :bind
  "add_unscaled_billboard" :hash 520007164)
 :void (material material) (default-scale float) (modulate color))

(defgmethod
 (editor-node-3dgizmo+add-handles :class 'editor-node-3dgizmo :bind
  "add_handles" :hash 2254560097)
 :void (handles packed-vector-3array) (material material)
 (ids packed-int-32array) (billboard bool) (secondary bool))

(defgmethod
 (editor-node-3dgizmo+set-node-3d :class 'editor-node-3dgizmo :bind
  "set_node_3d" :hash 1078189570)
 :void (node node))

(defgmethod
 (editor-node-3dgizmo+get-node-3d :class 'editor-node-3dgizmo :bind
  "get_node_3d" :hash 151077316)
 node-3d)

(defgmethod
 (editor-node-3dgizmo+get-plugin :class 'editor-node-3dgizmo :bind "get_plugin"
  :hash 4250544552)
 editor-node-3dgizmo-plugin)

(defgmethod
 (editor-node-3dgizmo+clear :class 'editor-node-3dgizmo :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (editor-node-3dgizmo+set-hidden :class 'editor-node-3dgizmo :bind "set_hidden"
  :hash 2586408642)
 :void (hidden bool))

(defgmethod
 (editor-node-3dgizmo+is-subgizmo-selected :class 'editor-node-3dgizmo :bind
  "is_subgizmo_selected" :hash 1116898809)
 bool (id int))

(defgmethod
 (editor-node-3dgizmo+get-subgizmo-selection :class 'editor-node-3dgizmo :bind
  "get_subgizmo_selection" :hash 1930428628)
 packed-int-32array)