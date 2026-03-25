(common-lisp:in-package :%godot)


(defgmethod
 (node-3d+set-transform :class 'node-3d :bind "set_transform" :hash 2952846383)
 :void (local transform-3d))

(defgmethod
 (node-3d+get-transform :class 'node-3d :bind "get_transform" :hash 3229777777)
 transform-3d)

(defgmethod
 (node-3d+set-position :class 'node-3d :bind "set_position" :hash 3460891852)
 :void (position vector-3))

(defgmethod
 (node-3d+get-position :class 'node-3d :bind "get_position" :hash 3360562783)
 vector-3)

(defgmethod
 (node-3d+set-rotation :class 'node-3d :bind "set_rotation" :hash 3460891852)
 :void (euler-radians vector-3))

(defgmethod
 (node-3d+get-rotation :class 'node-3d :bind "get_rotation" :hash 3360562783)
 vector-3)

(defgmethod
 (node-3d+set-rotation-degrees :class 'node-3d :bind "set_rotation_degrees"
  :hash 3460891852)
 :void (euler-degrees vector-3))

(defgmethod
 (node-3d+get-rotation-degrees :class 'node-3d :bind "get_rotation_degrees"
  :hash 3360562783)
 vector-3)

(defgmethod
 (node-3d+set-rotation-order :class 'node-3d :bind "set_rotation_order" :hash
  1820889989)
 :void (order euler-order))

(defgmethod
 (node-3d+get-rotation-order :class 'node-3d :bind "get_rotation_order" :hash
  916939469)
 euler-order)

(defgmethod
 (node-3d+set-rotation-edit-mode :class 'node-3d :bind "set_rotation_edit_mode"
  :hash 141483330)
 :void (edit-mode node-3d+rotation-edit-mode))

(defgmethod
 (node-3d+get-rotation-edit-mode :class 'node-3d :bind "get_rotation_edit_mode"
  :hash 1572188370)
 node-3d+rotation-edit-mode)

(defgmethod
 (node-3d+set-scale :class 'node-3d :bind "set_scale" :hash 3460891852) :void
 (scale vector-3))

(defgmethod
 (node-3d+get-scale :class 'node-3d :bind "get_scale" :hash 3360562783)
 vector-3)

(defgmethod
 (node-3d+set-quaternion :class 'node-3d :bind "set_quaternion" :hash
  1727505552)
 :void (quaternion quaternion))

(defgmethod
 (node-3d+get-quaternion :class 'node-3d :bind "get_quaternion" :hash
  1222331677)
 quaternion)

(defgmethod
 (node-3d+set-basis :class 'node-3d :bind "set_basis" :hash 1055510324) :void
 (basis basis))

(defgmethod
 (node-3d+get-basis :class 'node-3d :bind "get_basis" :hash 2716978435) basis)

(defgmethod
 (node-3d+set-global-transform :class 'node-3d :bind "set_global_transform"
  :hash 2952846383)
 :void (global transform-3d))

(defgmethod
 (node-3d+get-global-transform :class 'node-3d :bind "get_global_transform"
  :hash 3229777777)
 transform-3d)

(defgmethod
 (node-3d+get-global-transform-interpolated :class 'node-3d :bind
  "get_global_transform_interpolated" :hash 4183770049)
 transform-3d)

(defgmethod
 (node-3d+set-global-position :class 'node-3d :bind "set_global_position" :hash
  3460891852)
 :void (position vector-3))

(defgmethod
 (node-3d+get-global-position :class 'node-3d :bind "get_global_position" :hash
  3360562783)
 vector-3)

(defgmethod
 (node-3d+set-global-basis :class 'node-3d :bind "set_global_basis" :hash
  1055510324)
 :void (basis basis))

(defgmethod
 (node-3d+get-global-basis :class 'node-3d :bind "get_global_basis" :hash
  2716978435)
 basis)

(defgmethod
 (node-3d+set-global-rotation :class 'node-3d :bind "set_global_rotation" :hash
  3460891852)
 :void (euler-radians vector-3))

(defgmethod
 (node-3d+get-global-rotation :class 'node-3d :bind "get_global_rotation" :hash
  3360562783)
 vector-3)

(defgmethod
 (node-3d+set-global-rotation-degrees :class 'node-3d :bind
  "set_global_rotation_degrees" :hash 3460891852)
 :void (euler-degrees vector-3))

(defgmethod
 (node-3d+get-global-rotation-degrees :class 'node-3d :bind
  "get_global_rotation_degrees" :hash 3360562783)
 vector-3)

(defgmethod
 (node-3d+get-parent-node-3d :class 'node-3d :bind "get_parent_node_3d" :hash
  151077316)
 node-3d)

(defgmethod
 (node-3d+set-ignore-transform-notification :class 'node-3d :bind
  "set_ignore_transform_notification" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (node-3d+set-as-top-level :class 'node-3d :bind "set_as_top_level" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (node-3d+is-set-as-top-level :class 'node-3d :bind "is_set_as_top_level" :hash
  36873697)
 bool)

(defgmethod
 (node-3d+set-disable-scale :class 'node-3d :bind "set_disable_scale" :hash
  2586408642)
 :void (disable bool))

(defgmethod
 (node-3d+is-scale-disabled :class 'node-3d :bind "is_scale_disabled" :hash
  36873697)
 bool)

(defgmethod
 (node-3d+get-world-3d :class 'node-3d :bind "get_world_3d" :hash 317588385)
 world-3d)

(defgmethod
 (node-3d+force-update-transform :class 'node-3d :bind "force_update_transform"
  :hash 3218959716)
 :void)

(defgmethod
 (node-3d+set-visibility-parent :class 'node-3d :bind "set_visibility_parent"
  :hash 1348162250)
 :void (path node-path))

(defgmethod
 (node-3d+get-visibility-parent :class 'node-3d :bind "get_visibility_parent"
  :hash 4075236667)
 node-path)

(defgmethod
 (node-3d+update-gizmos :class 'node-3d :bind "update_gizmos" :hash 3218959716)
 :void)

(defgmethod
 (node-3d+add-gizmo :class 'node-3d :bind "add_gizmo" :hash 1544533845) :void
 (gizmo node-3dgizmo))

(defgmethod
 (node-3d+get-gizmos :class 'node-3d :bind "get_gizmos" :hash 3995934104) array)

(defgmethod
 (node-3d+clear-gizmos :class 'node-3d :bind "clear_gizmos" :hash 3218959716)
 :void)

(defgmethod
 (node-3d+set-subgizmo-selection :class 'node-3d :bind "set_subgizmo_selection"
  :hash 3317607635)
 :void (gizmo node-3dgizmo) (id int) (transform transform-3d))

(defgmethod
 (node-3d+clear-subgizmo-selection :class 'node-3d :bind
  "clear_subgizmo_selection" :hash 3218959716)
 :void)

(defgmethod
 (node-3d+set-visible :class 'node-3d :bind "set_visible" :hash 2586408642)
 :void (visible bool))

(defgmethod
 (node-3d+is-visible :class 'node-3d :bind "is_visible" :hash 36873697) bool)

(defgmethod
 (node-3d+is-visible-in-tree :class 'node-3d :bind "is_visible_in_tree" :hash
  36873697)
 bool)

(defgmethod (node-3d+show :class 'node-3d :bind "show" :hash 3218959716) :void)

(defgmethod (node-3d+hide :class 'node-3d :bind "hide" :hash 3218959716) :void)

(defgmethod
 (node-3d+set-notify-local-transform :class 'node-3d :bind
  "set_notify_local_transform" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (node-3d+is-local-transform-notification-enabled :class 'node-3d :bind
  "is_local_transform_notification_enabled" :hash 36873697)
 bool)

(defgmethod
 (node-3d+set-notify-transform :class 'node-3d :bind "set_notify_transform"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (node-3d+is-transform-notification-enabled :class 'node-3d :bind
  "is_transform_notification_enabled" :hash 36873697)
 bool)

(defgmethod (node-3d+rotate :class 'node-3d :bind "rotate" :hash 3436291937)
 :void (axis vector-3) (angle float))

(defgmethod
 (node-3d+global-rotate :class 'node-3d :bind "global_rotate" :hash 3436291937)
 :void (axis vector-3) (angle float))

(defgmethod
 (node-3d+global-scale :class 'node-3d :bind "global_scale" :hash 3460891852)
 :void (scale vector-3))

(defgmethod
 (node-3d+global-translate :class 'node-3d :bind "global_translate" :hash
  3460891852)
 :void (offset vector-3))

(defgmethod
 (node-3d+rotate-object-local :class 'node-3d :bind "rotate_object_local" :hash
  3436291937)
 :void (axis vector-3) (angle float))

(defgmethod
 (node-3d+scale-object-local :class 'node-3d :bind "scale_object_local" :hash
  3460891852)
 :void (scale vector-3))

(defgmethod
 (node-3d+translate-object-local :class 'node-3d :bind "translate_object_local"
  :hash 3460891852)
 :void (offset vector-3))

(defgmethod (node-3d+rotate-x :class 'node-3d :bind "rotate_x" :hash 373806689)
 :void (angle float))

(defgmethod (node-3d+rotate-y :class 'node-3d :bind "rotate_y" :hash 373806689)
 :void (angle float))

(defgmethod (node-3d+rotate-z :class 'node-3d :bind "rotate_z" :hash 373806689)
 :void (angle float))

(defgmethod
 (node-3d+translate :class 'node-3d :bind "translate" :hash 3460891852) :void
 (offset vector-3))

(defgmethod
 (node-3d+orthonormalize :class 'node-3d :bind "orthonormalize" :hash
  3218959716)
 :void)

(defgmethod
 (node-3d+set-identity :class 'node-3d :bind "set_identity" :hash 3218959716)
 :void)

(defgmethod (node-3d+look-at :class 'node-3d :bind "look_at" :hash 2882425029)
 :void (target vector-3) (up vector-3) (use-model-front bool))

(defgmethod
 (node-3d+look-at-from-position :class 'node-3d :bind "look_at_from_position"
  :hash 2086826090)
 :void (position vector-3) (target vector-3) (up vector-3)
 (use-model-front bool))

(defgmethod (node-3d+to-local :class 'node-3d :bind "to_local" :hash 192990374)
 vector-3 (global-point vector-3))

(defgmethod
 (node-3d+to-global :class 'node-3d :bind "to_global" :hash 192990374) vector-3
 (local-point vector-3))