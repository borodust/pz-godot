(common-lisp:in-package :%godot)


(defgmethod
 (editor-node-3dgizmo-plugin+%has-gizmo :class 'editor-node-3dgizmo-plugin
  :bind "_has_gizmo" :hash 1905827158 :virtual common-lisp:t)
 bool (for-node-3d node-3d))

(defgmethod
 (editor-node-3dgizmo-plugin+%create-gizmo :class 'editor-node-3dgizmo-plugin
  :bind "_create_gizmo" :hash 1418965287 :virtual common-lisp:t)
 editor-node-3dgizmo (for-node-3d node-3d))

(defgmethod
 (editor-node-3dgizmo-plugin+%get-gizmo-name :class 'editor-node-3dgizmo-plugin
  :bind "_get_gizmo_name" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-node-3dgizmo-plugin+%get-priority :class 'editor-node-3dgizmo-plugin
  :bind "_get_priority" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (editor-node-3dgizmo-plugin+%can-be-hidden :class 'editor-node-3dgizmo-plugin
  :bind "_can_be_hidden" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-node-3dgizmo-plugin+%is-selectable-when-hidden :class
  'editor-node-3dgizmo-plugin :bind "_is_selectable_when_hidden" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-node-3dgizmo-plugin+%redraw :class 'editor-node-3dgizmo-plugin :bind
  "_redraw" :hash 173330131 :virtual common-lisp:t)
 :void (gizmo editor-node-3dgizmo))

(defgmethod
 (editor-node-3dgizmo-plugin+%get-handle-name :class
  'editor-node-3dgizmo-plugin :bind "_get_handle_name" :hash 3888674840
  :virtual common-lisp:t)
 string (gizmo editor-node-3dgizmo) (handle-id int) (secondary bool))

(defgmethod
 (editor-node-3dgizmo-plugin+%is-handle-highlighted :class
  'editor-node-3dgizmo-plugin :bind "_is_handle_highlighted" :hash 2665780718
  :virtual common-lisp:t)
 bool (gizmo editor-node-3dgizmo) (handle-id int) (secondary bool))

(defgmethod
 (editor-node-3dgizmo-plugin+%get-handle-value :class
  'editor-node-3dgizmo-plugin :bind "_get_handle_value" :hash 2887724832
  :virtual common-lisp:t)
 variant (gizmo editor-node-3dgizmo) (handle-id int) (secondary bool))

(defgmethod
 (editor-node-3dgizmo-plugin+%begin-handle-action :class
  'editor-node-3dgizmo-plugin :bind "_begin_handle_action" :hash 3363704593
  :virtual common-lisp:t)
 :void (gizmo editor-node-3dgizmo) (handle-id int) (secondary bool))

(defgmethod
 (editor-node-3dgizmo-plugin+%set-handle :class 'editor-node-3dgizmo-plugin
  :bind "_set_handle" :hash 1249646868 :virtual common-lisp:t)
 :void (gizmo editor-node-3dgizmo) (handle-id int) (secondary bool)
 (camera camera-3d) (screen-pos vector-2))

(defgmethod
 (editor-node-3dgizmo-plugin+%commit-handle :class 'editor-node-3dgizmo-plugin
  :bind "_commit_handle" :hash 1939863962 :virtual common-lisp:t)
 :void (gizmo editor-node-3dgizmo) (handle-id int) (secondary bool)
 (restore variant) (cancel bool))

(defgmethod
 (editor-node-3dgizmo-plugin+%subgizmos-intersect-ray :class
  'editor-node-3dgizmo-plugin :bind "_subgizmos_intersect_ray" :hash 1781916302
  :virtual common-lisp:t)
 int (gizmo editor-node-3dgizmo) (camera camera-3d) (screen-pos vector-2))

(defgmethod
 (editor-node-3dgizmo-plugin+%subgizmos-intersect-frustum :class
  'editor-node-3dgizmo-plugin :bind "_subgizmos_intersect_frustum" :hash
  3514748524 :virtual common-lisp:t)
 packed-int-32array (gizmo editor-node-3dgizmo) (camera camera-3d)
 (frustum-planes array))

(defgmethod
 (editor-node-3dgizmo-plugin+%get-subgizmo-transform :class
  'editor-node-3dgizmo-plugin :bind "_get_subgizmo_transform" :hash 3700343508
  :virtual common-lisp:t)
 transform-3d (gizmo editor-node-3dgizmo) (subgizmo-id int))

(defgmethod
 (editor-node-3dgizmo-plugin+%set-subgizmo-transform :class
  'editor-node-3dgizmo-plugin :bind "_set_subgizmo_transform" :hash 2435388792
  :virtual common-lisp:t)
 :void (gizmo editor-node-3dgizmo) (subgizmo-id int) (transform transform-3d))

(defgmethod
 (editor-node-3dgizmo-plugin+%commit-subgizmos :class
  'editor-node-3dgizmo-plugin :bind "_commit_subgizmos" :hash 2282018236
  :virtual common-lisp:t)
 :void (gizmo editor-node-3dgizmo) (ids packed-int-32array) (restores array)
 (cancel bool))

(defgmethod
 (editor-node-3dgizmo-plugin+create-material :class 'editor-node-3dgizmo-plugin
  :bind "create_material" :hash 3486012546)
 :void (name string) (color color) (billboard bool) (on-top bool)
 (use-vertex-color bool))

(defgmethod
 (editor-node-3dgizmo-plugin+create-icon-material :class
  'editor-node-3dgizmo-plugin :bind "create_icon_material" :hash 3804976916)
 :void (name string) (texture texture-2d) (on-top bool) (color color))

(defgmethod
 (editor-node-3dgizmo-plugin+create-handle-material :class
  'editor-node-3dgizmo-plugin :bind "create_handle_material" :hash 2486475223)
 :void (name string) (billboard bool) (texture texture-2d))

(defgmethod
 (editor-node-3dgizmo-plugin+add-material :class 'editor-node-3dgizmo-plugin
  :bind "add_material" :hash 1374068695)
 :void (name string) (material standard-material-3d))

(defgmethod
 (editor-node-3dgizmo-plugin+get-material :class 'editor-node-3dgizmo-plugin
  :bind "get_material" :hash 974464017)
 standard-material-3d (name string) (gizmo editor-node-3dgizmo))