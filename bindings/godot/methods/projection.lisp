(common-lisp:in-package :%godot)


(defgmethod
 (projection+create-depth-correction :class 'projection :bind
  "create_depth_correction" :hash 1228516048 :static common-lisp:t)
 projection (flip-y bool))

(defgmethod
 (projection+create-light-atlas-rect :class 'projection :bind
  "create_light_atlas_rect" :hash 2654950662 :static common-lisp:t)
 projection (rect rect-2))

(defgmethod
 (projection+create-perspective :class 'projection :bind "create_perspective"
  :hash 390915442 :static common-lisp:t)
 projection (fovy float) (aspect float) (z-near float) (z-far float)
 (flip-fov bool))

(defgmethod
 (projection+create-perspective-hmd :class 'projection :bind
  "create_perspective_hmd" :hash 2857674800 :static common-lisp:t)
 projection (fovy float) (aspect float) (z-near float) (z-far float)
 (flip-fov bool) (eye int) (intraocular-dist float) (convergence-dist float))

(defgmethod
 (projection+create-for-hmd :class 'projection :bind "create_for_hmd" :hash
  4184144994 :static common-lisp:t)
 projection (eye int) (aspect float) (intraocular-dist float)
 (display-width float) (display-to-lens float) (oversample float)
 (z-near float) (z-far float))

(defgmethod
 (projection+create-orthogonal :class 'projection :bind "create_orthogonal"
  :hash 3707929169 :static common-lisp:t)
 projection (left float) (right float) (bottom float) (top float)
 (z-near float) (z-far float))

(defgmethod
 (projection+create-orthogonal-aspect :class 'projection :bind
  "create_orthogonal_aspect" :hash 390915442 :static common-lisp:t)
 projection (size float) (aspect float) (z-near float) (z-far float)
 (flip-fov bool))

(defgmethod
 (projection+create-frustum :class 'projection :bind "create_frustum" :hash
  3707929169 :static common-lisp:t)
 projection (left float) (right float) (bottom float) (top float)
 (z-near float) (z-far float))

(defgmethod
 (projection+create-frustum-aspect :class 'projection :bind
  "create_frustum_aspect" :hash 1535076251 :static common-lisp:t)
 projection (size float) (aspect float) (offset vector-2) (z-near float)
 (z-far float) (flip-fov bool))

(defgmethod
 (projection+create-fit-aabb :class 'projection :bind "create_fit_aabb" :hash
  2264694907 :static common-lisp:t)
 projection (aabb aabb))

(defgmethod
 (projection+determinant :class 'projection :bind "determinant" :hash
  466405837)
 float)

(defgmethod
 (projection+perspective-znear-adjusted :class 'projection :bind
  "perspective_znear_adjusted" :hash 3584785443)
 projection (new-znear float))

(defgmethod
 (projection+get-projection-plane :class 'projection :bind
  "get_projection_plane" :hash 1551184160)
 plane (plane int))

(defgmethod
 (projection+flipped-y :class 'projection :bind "flipped_y" :hash 4212530932)
 projection)

(defgmethod
 (projection+jitter-offseted :class 'projection :bind "jitter_offseted" :hash
  2448438599)
 projection (offset vector-2))

(defgmethod
 (projection+get-fovy :class 'projection :bind "get_fovy" :hash 3514207532
  :static common-lisp:t)
 float (fovx float) (aspect float))

(defgmethod
 (projection+get-z-far :class 'projection :bind "get_z_far" :hash 466405837)
 float)

(defgmethod
 (projection+get-z-near :class 'projection :bind "get_z_near" :hash 466405837)
 float)

(defgmethod
 (projection+get-aspect :class 'projection :bind "get_aspect" :hash 466405837)
 float)

(defgmethod
 (projection+get-fov :class 'projection :bind "get_fov" :hash 466405837) float)

(defgmethod
 (projection+is-orthogonal :class 'projection :bind "is_orthogonal" :hash
  3918633141)
 bool)

(defgmethod
 (projection+get-viewport-half-extents :class 'projection :bind
  "get_viewport_half_extents" :hash 2428350749)
 vector-2)

(defgmethod
 (projection+get-far-plane-half-extents :class 'projection :bind
  "get_far_plane_half_extents" :hash 2428350749)
 vector-2)

(defgmethod
 (projection+inverse :class 'projection :bind "inverse" :hash 4212530932)
 projection)

(defgmethod
 (projection+get-pixels-per-meter :class 'projection :bind
  "get_pixels_per_meter" :hash 4103005248)
 int (for-pixel-width int))

(defgmethod
 (projection+get-lod-multiplier :class 'projection :bind "get_lod_multiplier"
  :hash 466405837)
 float)