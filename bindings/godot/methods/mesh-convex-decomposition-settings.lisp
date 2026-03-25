(common-lisp:in-package :%godot)


(defgmethod
 (mesh-convex-decomposition-settings+set-max-concavity :class
  'mesh-convex-decomposition-settings :bind "set_max_concavity" :hash
  373806689)
 :void (max-concavity float))

(defgmethod
 (mesh-convex-decomposition-settings+get-max-concavity :class
  'mesh-convex-decomposition-settings :bind "get_max_concavity" :hash
  1740695150)
 float)

(defgmethod
 (mesh-convex-decomposition-settings+set-symmetry-planes-clipping-bias :class
  'mesh-convex-decomposition-settings :bind "set_symmetry_planes_clipping_bias"
  :hash 373806689)
 :void (symmetry-planes-clipping-bias float))

(defgmethod
 (mesh-convex-decomposition-settings+get-symmetry-planes-clipping-bias :class
  'mesh-convex-decomposition-settings :bind "get_symmetry_planes_clipping_bias"
  :hash 1740695150)
 float)

(defgmethod
 (mesh-convex-decomposition-settings+set-revolution-axes-clipping-bias :class
  'mesh-convex-decomposition-settings :bind "set_revolution_axes_clipping_bias"
  :hash 373806689)
 :void (revolution-axes-clipping-bias float))

(defgmethod
 (mesh-convex-decomposition-settings+get-revolution-axes-clipping-bias :class
  'mesh-convex-decomposition-settings :bind "get_revolution_axes_clipping_bias"
  :hash 1740695150)
 float)

(defgmethod
 (mesh-convex-decomposition-settings+set-min-volume-per-convex-hull :class
  'mesh-convex-decomposition-settings :bind "set_min_volume_per_convex_hull"
  :hash 373806689)
 :void (min-volume-per-convex-hull float))

(defgmethod
 (mesh-convex-decomposition-settings+get-min-volume-per-convex-hull :class
  'mesh-convex-decomposition-settings :bind "get_min_volume_per_convex_hull"
  :hash 1740695150)
 float)

(defgmethod
 (mesh-convex-decomposition-settings+set-resolution :class
  'mesh-convex-decomposition-settings :bind "set_resolution" :hash 1286410249)
 :void (min-volume-per-convex-hull int))

(defgmethod
 (mesh-convex-decomposition-settings+get-resolution :class
  'mesh-convex-decomposition-settings :bind "get_resolution" :hash 3905245786)
 int)

(defgmethod
 (mesh-convex-decomposition-settings+set-max-num-vertices-per-convex-hull
  :class 'mesh-convex-decomposition-settings :bind
  "set_max_num_vertices_per_convex_hull" :hash 1286410249)
 :void (max-num-vertices-per-convex-hull int))

(defgmethod
 (mesh-convex-decomposition-settings+get-max-num-vertices-per-convex-hull
  :class 'mesh-convex-decomposition-settings :bind
  "get_max_num_vertices_per_convex_hull" :hash 3905245786)
 int)

(defgmethod
 (mesh-convex-decomposition-settings+set-plane-downsampling :class
  'mesh-convex-decomposition-settings :bind "set_plane_downsampling" :hash
  1286410249)
 :void (plane-downsampling int))

(defgmethod
 (mesh-convex-decomposition-settings+get-plane-downsampling :class
  'mesh-convex-decomposition-settings :bind "get_plane_downsampling" :hash
  3905245786)
 int)

(defgmethod
 (mesh-convex-decomposition-settings+set-convex-hull-downsampling :class
  'mesh-convex-decomposition-settings :bind "set_convex_hull_downsampling"
  :hash 1286410249)
 :void (convex-hull-downsampling int))

(defgmethod
 (mesh-convex-decomposition-settings+get-convex-hull-downsampling :class
  'mesh-convex-decomposition-settings :bind "get_convex_hull_downsampling"
  :hash 3905245786)
 int)

(defgmethod
 (mesh-convex-decomposition-settings+set-normalize-mesh :class
  'mesh-convex-decomposition-settings :bind "set_normalize_mesh" :hash
  2586408642)
 :void (normalize-mesh bool))

(defgmethod
 (mesh-convex-decomposition-settings+get-normalize-mesh :class
  'mesh-convex-decomposition-settings :bind "get_normalize_mesh" :hash
  36873697)
 bool)

(defgmethod
 (mesh-convex-decomposition-settings+set-mode :class
  'mesh-convex-decomposition-settings :bind "set_mode" :hash 1668072869)
 :void (mode mesh-convex-decomposition-settings+mode))

(defgmethod
 (mesh-convex-decomposition-settings+get-mode :class
  'mesh-convex-decomposition-settings :bind "get_mode" :hash 23479454)
 mesh-convex-decomposition-settings+mode)

(defgmethod
 (mesh-convex-decomposition-settings+set-convex-hull-approximation :class
  'mesh-convex-decomposition-settings :bind "set_convex_hull_approximation"
  :hash 2586408642)
 :void (convex-hull-approximation bool))

(defgmethod
 (mesh-convex-decomposition-settings+get-convex-hull-approximation :class
  'mesh-convex-decomposition-settings :bind "get_convex_hull_approximation"
  :hash 36873697)
 bool)

(defgmethod
 (mesh-convex-decomposition-settings+set-max-convex-hulls :class
  'mesh-convex-decomposition-settings :bind "set_max_convex_hulls" :hash
  1286410249)
 :void (max-convex-hulls int))

(defgmethod
 (mesh-convex-decomposition-settings+get-max-convex-hulls :class
  'mesh-convex-decomposition-settings :bind "get_max_convex_hulls" :hash
  3905245786)
 int)

(defgmethod
 (mesh-convex-decomposition-settings+set-project-hull-vertices :class
  'mesh-convex-decomposition-settings :bind "set_project_hull_vertices" :hash
  2586408642)
 :void (project-hull-vertices bool))

(defgmethod
 (mesh-convex-decomposition-settings+get-project-hull-vertices :class
  'mesh-convex-decomposition-settings :bind "get_project_hull_vertices" :hash
  36873697)
 bool)