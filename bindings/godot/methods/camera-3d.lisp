(common-lisp:in-package :%godot)


(defgmethod
 (camera-3d+project-ray-normal :class 'camera-3d :bind "project_ray_normal"
  :hash 1718073306)
 vector-3 (screen-point vector-2))

(defgmethod
 (camera-3d+project-local-ray-normal :class 'camera-3d :bind
  "project_local_ray_normal" :hash 1718073306)
 vector-3 (screen-point vector-2))

(defgmethod
 (camera-3d+project-ray-origin :class 'camera-3d :bind "project_ray_origin"
  :hash 1718073306)
 vector-3 (screen-point vector-2))

(defgmethod
 (camera-3d+unproject-position :class 'camera-3d :bind "unproject_position"
  :hash 3758901831)
 vector-2 (world-point vector-3))

(defgmethod
 (camera-3d+is-position-behind :class 'camera-3d :bind "is_position_behind"
  :hash 3108956480)
 bool (world-point vector-3))

(defgmethod
 (camera-3d+project-position :class 'camera-3d :bind "project_position" :hash
  2171975744)
 vector-3 (screen-point vector-2) (z-depth float))

(defgmethod
 (camera-3d+set-perspective :class 'camera-3d :bind "set_perspective" :hash
  2385087082)
 :void (fov float) (z-near float) (z-far float))

(defgmethod
 (camera-3d+set-orthogonal :class 'camera-3d :bind "set_orthogonal" :hash
  2385087082)
 :void (size float) (z-near float) (z-far float))

(defgmethod
 (camera-3d+set-frustum :class 'camera-3d :bind "set_frustum" :hash 354890663)
 :void (size float) (offset vector-2) (z-near float) (z-far float))

(defgmethod
 (camera-3d+make-current :class 'camera-3d :bind "make_current" :hash
  3218959716)
 :void)

(defgmethod
 (camera-3d+clear-current :class 'camera-3d :bind "clear_current" :hash
  3216645846)
 :void (enable-next bool))

(defgmethod
 (camera-3d+set-current :class 'camera-3d :bind "set_current" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (camera-3d+is-current :class 'camera-3d :bind "is_current" :hash 36873697)
 bool)

(defgmethod
 (camera-3d+get-camera-transform :class 'camera-3d :bind "get_camera_transform"
  :hash 3229777777)
 transform-3d)

(defgmethod
 (camera-3d+get-camera-projection :class 'camera-3d :bind
  "get_camera_projection" :hash 2910717950)
 projection)

(defgmethod
 (camera-3d+get-fov :class 'camera-3d :bind "get_fov" :hash 1740695150) float)

(defgmethod
 (camera-3d+get-frustum-offset :class 'camera-3d :bind "get_frustum_offset"
  :hash 3341600327)
 vector-2)

(defgmethod
 (camera-3d+get-size :class 'camera-3d :bind "get_size" :hash 1740695150) float)

(defgmethod
 (camera-3d+get-far :class 'camera-3d :bind "get_far" :hash 1740695150) float)

(defgmethod
 (camera-3d+get-near :class 'camera-3d :bind "get_near" :hash 1740695150) float)

(defgmethod
 (camera-3d+set-fov :class 'camera-3d :bind "set_fov" :hash 373806689) :void
 (fov float))

(defgmethod
 (camera-3d+set-frustum-offset :class 'camera-3d :bind "set_frustum_offset"
  :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (camera-3d+set-size :class 'camera-3d :bind "set_size" :hash 373806689) :void
 (size float))

(defgmethod
 (camera-3d+set-far :class 'camera-3d :bind "set_far" :hash 373806689) :void
 (far float))

(defgmethod
 (camera-3d+set-near :class 'camera-3d :bind "set_near" :hash 373806689) :void
 (near float))

(defgmethod
 (camera-3d+get-projection :class 'camera-3d :bind "get_projection" :hash
  2624185235)
 camera-3d+projection-type)

(defgmethod
 (camera-3d+set-projection :class 'camera-3d :bind "set_projection" :hash
  4218540108)
 :void (mode camera-3d+projection-type))

(defgmethod
 (camera-3d+set-h-offset :class 'camera-3d :bind "set_h_offset" :hash
  373806689)
 :void (offset float))

(defgmethod
 (camera-3d+get-h-offset :class 'camera-3d :bind "get_h_offset" :hash
  1740695150)
 float)

(defgmethod
 (camera-3d+set-v-offset :class 'camera-3d :bind "set_v_offset" :hash
  373806689)
 :void (offset float))

(defgmethod
 (camera-3d+get-v-offset :class 'camera-3d :bind "get_v_offset" :hash
  1740695150)
 float)

(defgmethod
 (camera-3d+set-cull-mask :class 'camera-3d :bind "set_cull_mask" :hash
  1286410249)
 :void (mask int))

(defgmethod
 (camera-3d+get-cull-mask :class 'camera-3d :bind "get_cull_mask" :hash
  3905245786)
 int)

(defgmethod
 (camera-3d+set-environment :class 'camera-3d :bind "set_environment" :hash
  4143518816)
 :void (env environment))

(defgmethod
 (camera-3d+get-environment :class 'camera-3d :bind "get_environment" :hash
  3082064660)
 environment)

(defgmethod
 (camera-3d+set-attributes :class 'camera-3d :bind "set_attributes" :hash
  2817810567)
 :void (env camera-attributes))

(defgmethod
 (camera-3d+get-attributes :class 'camera-3d :bind "get_attributes" :hash
  3921283215)
 camera-attributes)

(defgmethod
 (camera-3d+set-compositor :class 'camera-3d :bind "set_compositor" :hash
  1586754307)
 :void (compositor compositor))

(defgmethod
 (camera-3d+get-compositor :class 'camera-3d :bind "get_compositor" :hash
  3647707413)
 compositor)

(defgmethod
 (camera-3d+set-keep-aspect-mode :class 'camera-3d :bind "set_keep_aspect_mode"
  :hash 1740651252)
 :void (mode camera-3d+keep-aspect))

(defgmethod
 (camera-3d+get-keep-aspect-mode :class 'camera-3d :bind "get_keep_aspect_mode"
  :hash 2790278316)
 camera-3d+keep-aspect)

(defgmethod
 (camera-3d+set-doppler-tracking :class 'camera-3d :bind "set_doppler_tracking"
  :hash 3109431270)
 :void (mode camera-3d+doppler-tracking))

(defgmethod
 (camera-3d+get-doppler-tracking :class 'camera-3d :bind "get_doppler_tracking"
  :hash 1584483649)
 camera-3d+doppler-tracking)

(defgmethod
 (camera-3d+get-frustum :class 'camera-3d :bind "get_frustum" :hash 3995934104)
 array)

(defgmethod
 (camera-3d+is-position-in-frustum :class 'camera-3d :bind
  "is_position_in_frustum" :hash 3108956480)
 bool (world-point vector-3))

(defgmethod
 (camera-3d+get-camera-rid :class 'camera-3d :bind "get_camera_rid" :hash
  2944877500)
 rid)

(defgmethod
 (camera-3d+get-pyramid-shape-rid :class 'camera-3d :bind
  "get_pyramid_shape_rid" :hash 529393457)
 rid)

(defgmethod
 (camera-3d+set-cull-mask-value :class 'camera-3d :bind "set_cull_mask_value"
  :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (camera-3d+get-cull-mask-value :class 'camera-3d :bind "get_cull_mask_value"
  :hash 1116898809)
 bool (layer-number int))