(common-lisp:in-package :%godot)


(defgmethod
 (mesh-instance-3d+set-mesh :class 'mesh-instance-3d :bind "set_mesh" :hash
  194775623)
 :void (mesh mesh))

(defgmethod
 (mesh-instance-3d+get-mesh :class 'mesh-instance-3d :bind "get_mesh" :hash
  1808005922)
 mesh)

(defgmethod
 (mesh-instance-3d+set-skeleton-path :class 'mesh-instance-3d :bind
  "set_skeleton_path" :hash 1348162250)
 :void (skeleton-path node-path))

(defgmethod
 (mesh-instance-3d+get-skeleton-path :class 'mesh-instance-3d :bind
  "get_skeleton_path" :hash 277076166)
 node-path)

(defgmethod
 (mesh-instance-3d+set-skin :class 'mesh-instance-3d :bind "set_skin" :hash
  3971435618)
 :void (skin skin))

(defgmethod
 (mesh-instance-3d+get-skin :class 'mesh-instance-3d :bind "get_skin" :hash
  2074563878)
 skin)

(defgmethod
 (mesh-instance-3d+get-skin-reference :class 'mesh-instance-3d :bind
  "get_skin_reference" :hash 2060603409)
 skin-reference)

(defgmethod
 (mesh-instance-3d+get-surface-override-material-count :class 'mesh-instance-3d
  :bind "get_surface_override_material_count" :hash 3905245786)
 int)

(defgmethod
 (mesh-instance-3d+set-surface-override-material :class 'mesh-instance-3d :bind
  "set_surface_override_material" :hash 3671737478)
 :void (surface int) (material material))

(defgmethod
 (mesh-instance-3d+get-surface-override-material :class 'mesh-instance-3d :bind
  "get_surface_override_material" :hash 2897466400)
 material (surface int))

(defgmethod
 (mesh-instance-3d+get-active-material :class 'mesh-instance-3d :bind
  "get_active_material" :hash 2897466400)
 material (surface int))

(defgmethod
 (mesh-instance-3d+create-trimesh-collision :class 'mesh-instance-3d :bind
  "create_trimesh_collision" :hash 3218959716)
 :void)

(defgmethod
 (mesh-instance-3d+create-convex-collision :class 'mesh-instance-3d :bind
  "create_convex_collision" :hash 2751962654)
 :void (clean bool) (simplify bool))

(defgmethod
 (mesh-instance-3d+create-multiple-convex-collisions :class 'mesh-instance-3d
  :bind "create_multiple_convex_collisions" :hash 628789669)
 :void (settings mesh-convex-decomposition-settings))

(defgmethod
 (mesh-instance-3d+get-blend-shape-count :class 'mesh-instance-3d :bind
  "get_blend_shape_count" :hash 3905245786)
 int)

(defgmethod
 (mesh-instance-3d+find-blend-shape-by-name :class 'mesh-instance-3d :bind
  "find_blend_shape_by_name" :hash 4150868206)
 int (name string-name))

(defgmethod
 (mesh-instance-3d+get-blend-shape-value :class 'mesh-instance-3d :bind
  "get_blend_shape_value" :hash 2339986948)
 float (blend-shape-idx int))

(defgmethod
 (mesh-instance-3d+set-blend-shape-value :class 'mesh-instance-3d :bind
  "set_blend_shape_value" :hash 1602489585)
 :void (blend-shape-idx int) (value float))

(defgmethod
 (mesh-instance-3d+create-debug-tangents :class 'mesh-instance-3d :bind
  "create_debug_tangents" :hash 3218959716)
 :void)

(defgmethod
 (mesh-instance-3d+bake-mesh-from-current-blend-shape-mix :class
  'mesh-instance-3d :bind "bake_mesh_from_current_blend_shape_mix" :hash
  1457573577)
 array-mesh (existing array-mesh))

(defgmethod
 (mesh-instance-3d+bake-mesh-from-current-skeleton-pose :class
  'mesh-instance-3d :bind "bake_mesh_from_current_skeleton_pose" :hash
  1457573577)
 array-mesh (existing array-mesh))