(common-lisp:in-package :%godot)


(defgmethod
 (csgpolygon-3d+set-polygon :class 'csgpolygon-3d :bind "set_polygon" :hash
  1509147220)
 :void (polygon packed-vector-2array))

(defgmethod
 (csgpolygon-3d+get-polygon :class 'csgpolygon-3d :bind "get_polygon" :hash
  2961356807)
 packed-vector-2array)

(defgmethod
 (csgpolygon-3d+set-mode :class 'csgpolygon-3d :bind "set_mode" :hash
  3158377035)
 :void (mode csgpolygon-3d+mode))

(defgmethod
 (csgpolygon-3d+get-mode :class 'csgpolygon-3d :bind "get_mode" :hash
  1201612222)
 csgpolygon-3d+mode)

(defgmethod
 (csgpolygon-3d+set-depth :class 'csgpolygon-3d :bind "set_depth" :hash
  373806689)
 :void (depth float))

(defgmethod
 (csgpolygon-3d+get-depth :class 'csgpolygon-3d :bind "get_depth" :hash
  1740695150)
 float)

(defgmethod
 (csgpolygon-3d+set-spin-degrees :class 'csgpolygon-3d :bind "set_spin_degrees"
  :hash 373806689)
 :void (degrees float))

(defgmethod
 (csgpolygon-3d+get-spin-degrees :class 'csgpolygon-3d :bind "get_spin_degrees"
  :hash 1740695150)
 float)

(defgmethod
 (csgpolygon-3d+set-spin-sides :class 'csgpolygon-3d :bind "set_spin_sides"
  :hash 1286410249)
 :void (spin-sides int))

(defgmethod
 (csgpolygon-3d+get-spin-sides :class 'csgpolygon-3d :bind "get_spin_sides"
  :hash 3905245786)
 int)

(defgmethod
 (csgpolygon-3d+set-path-node :class 'csgpolygon-3d :bind "set_path_node" :hash
  1348162250)
 :void (path node-path))

(defgmethod
 (csgpolygon-3d+get-path-node :class 'csgpolygon-3d :bind "get_path_node" :hash
  4075236667)
 node-path)

(defgmethod
 (csgpolygon-3d+set-path-interval-type :class 'csgpolygon-3d :bind
  "set_path_interval_type" :hash 3744240707)
 :void (interval-type csgpolygon-3d+path-interval-type))

(defgmethod
 (csgpolygon-3d+get-path-interval-type :class 'csgpolygon-3d :bind
  "get_path_interval_type" :hash 3434618397)
 csgpolygon-3d+path-interval-type)

(defgmethod
 (csgpolygon-3d+set-path-interval :class 'csgpolygon-3d :bind
  "set_path_interval" :hash 373806689)
 :void (interval float))

(defgmethod
 (csgpolygon-3d+get-path-interval :class 'csgpolygon-3d :bind
  "get_path_interval" :hash 1740695150)
 float)

(defgmethod
 (csgpolygon-3d+set-path-simplify-angle :class 'csgpolygon-3d :bind
  "set_path_simplify_angle" :hash 373806689)
 :void (degrees float))

(defgmethod
 (csgpolygon-3d+get-path-simplify-angle :class 'csgpolygon-3d :bind
  "get_path_simplify_angle" :hash 1740695150)
 float)

(defgmethod
 (csgpolygon-3d+set-path-rotation :class 'csgpolygon-3d :bind
  "set_path_rotation" :hash 1412947288)
 :void (path-rotation csgpolygon-3d+path-rotation))

(defgmethod
 (csgpolygon-3d+get-path-rotation :class 'csgpolygon-3d :bind
  "get_path_rotation" :hash 647219346)
 csgpolygon-3d+path-rotation)

(defgmethod
 (csgpolygon-3d+set-path-rotation-accurate :class 'csgpolygon-3d :bind
  "set_path_rotation_accurate" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (csgpolygon-3d+get-path-rotation-accurate :class 'csgpolygon-3d :bind
  "get_path_rotation_accurate" :hash 36873697)
 bool)

(defgmethod
 (csgpolygon-3d+set-path-local :class 'csgpolygon-3d :bind "set_path_local"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (csgpolygon-3d+is-path-local :class 'csgpolygon-3d :bind "is_path_local" :hash
  36873697)
 bool)

(defgmethod
 (csgpolygon-3d+set-path-continuous-u :class 'csgpolygon-3d :bind
  "set_path_continuous_u" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (csgpolygon-3d+is-path-continuous-u :class 'csgpolygon-3d :bind
  "is_path_continuous_u" :hash 36873697)
 bool)

(defgmethod
 (csgpolygon-3d+set-path-u-distance :class 'csgpolygon-3d :bind
  "set_path_u_distance" :hash 373806689)
 :void (distance float))

(defgmethod
 (csgpolygon-3d+get-path-u-distance :class 'csgpolygon-3d :bind
  "get_path_u_distance" :hash 1740695150)
 float)

(defgmethod
 (csgpolygon-3d+set-path-joined :class 'csgpolygon-3d :bind "set_path_joined"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (csgpolygon-3d+is-path-joined :class 'csgpolygon-3d :bind "is_path_joined"
  :hash 36873697)
 bool)

(defgmethod
 (csgpolygon-3d+set-material :class 'csgpolygon-3d :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (csgpolygon-3d+get-material :class 'csgpolygon-3d :bind "get_material" :hash
  5934680)
 material)

(defgmethod
 (csgpolygon-3d+set-smooth-faces :class 'csgpolygon-3d :bind "set_smooth_faces"
  :hash 2586408642)
 :void (smooth-faces bool))

(defgmethod
 (csgpolygon-3d+get-smooth-faces :class 'csgpolygon-3d :bind "get_smooth_faces"
  :hash 36873697)
 bool)