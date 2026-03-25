(common-lisp:in-package :%godot)


(defgmethod
 (line-2d+set-points :class 'line-2d :bind "set_points" :hash 1509147220) :void
 (points packed-vector-2array))

(defgmethod
 (line-2d+get-points :class 'line-2d :bind "get_points" :hash 2961356807)
 packed-vector-2array)

(defgmethod
 (line-2d+set-point-position :class 'line-2d :bind "set_point_position" :hash
  163021252)
 :void (index int) (position vector-2))

(defgmethod
 (line-2d+get-point-position :class 'line-2d :bind "get_point_position" :hash
  2299179447)
 vector-2 (index int))

(defgmethod
 (line-2d+get-point-count :class 'line-2d :bind "get_point_count" :hash
  3905245786)
 int)

(defgmethod
 (line-2d+add-point :class 'line-2d :bind "add_point" :hash 2654014372) :void
 (position vector-2) (index int))

(defgmethod
 (line-2d+remove-point :class 'line-2d :bind "remove_point" :hash 1286410249)
 :void (index int))

(defgmethod
 (line-2d+clear-points :class 'line-2d :bind "clear_points" :hash 3218959716)
 :void)

(defgmethod
 (line-2d+set-closed :class 'line-2d :bind "set_closed" :hash 2586408642) :void
 (closed bool))

(defgmethod
 (line-2d+is-closed :class 'line-2d :bind "is_closed" :hash 36873697) bool)

(defgmethod
 (line-2d+set-width :class 'line-2d :bind "set_width" :hash 373806689) :void
 (width float))

(defgmethod
 (line-2d+get-width :class 'line-2d :bind "get_width" :hash 1740695150) float)

(defgmethod
 (line-2d+set-curve :class 'line-2d :bind "set_curve" :hash 270443179) :void
 (curve curve))

(defgmethod
 (line-2d+get-curve :class 'line-2d :bind "get_curve" :hash 2460114913) curve)

(defgmethod
 (line-2d+set-default-color :class 'line-2d :bind "set_default_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (line-2d+get-default-color :class 'line-2d :bind "get_default_color" :hash
  3444240500)
 color)

(defgmethod
 (line-2d+set-gradient :class 'line-2d :bind "set_gradient" :hash 2756054477)
 :void (color gradient))

(defgmethod
 (line-2d+get-gradient :class 'line-2d :bind "get_gradient" :hash 132272999)
 gradient)

(defgmethod
 (line-2d+set-texture :class 'line-2d :bind "set_texture" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (line-2d+get-texture :class 'line-2d :bind "get_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (line-2d+set-texture-mode :class 'line-2d :bind "set_texture_mode" :hash
  1952559516)
 :void (mode line-2d+line-texture-mode))

(defgmethod
 (line-2d+get-texture-mode :class 'line-2d :bind "get_texture_mode" :hash
  2341040722)
 line-2d+line-texture-mode)

(defgmethod
 (line-2d+set-joint-mode :class 'line-2d :bind "set_joint_mode" :hash
  604292979)
 :void (mode line-2d+line-joint-mode))

(defgmethod
 (line-2d+get-joint-mode :class 'line-2d :bind "get_joint_mode" :hash
  2546544037)
 line-2d+line-joint-mode)

(defgmethod
 (line-2d+set-begin-cap-mode :class 'line-2d :bind "set_begin_cap_mode" :hash
  1669024546)
 :void (mode line-2d+line-cap-mode))

(defgmethod
 (line-2d+get-begin-cap-mode :class 'line-2d :bind "get_begin_cap_mode" :hash
  1107511441)
 line-2d+line-cap-mode)

(defgmethod
 (line-2d+set-end-cap-mode :class 'line-2d :bind "set_end_cap_mode" :hash
  1669024546)
 :void (mode line-2d+line-cap-mode))

(defgmethod
 (line-2d+get-end-cap-mode :class 'line-2d :bind "get_end_cap_mode" :hash
  1107511441)
 line-2d+line-cap-mode)

(defgmethod
 (line-2d+set-sharp-limit :class 'line-2d :bind "set_sharp_limit" :hash
  373806689)
 :void (limit float))

(defgmethod
 (line-2d+get-sharp-limit :class 'line-2d :bind "get_sharp_limit" :hash
  1740695150)
 float)

(defgmethod
 (line-2d+set-round-precision :class 'line-2d :bind "set_round_precision" :hash
  1286410249)
 :void (precision int))

(defgmethod
 (line-2d+get-round-precision :class 'line-2d :bind "get_round_precision" :hash
  3905245786)
 int)

(defgmethod
 (line-2d+set-antialiased :class 'line-2d :bind "set_antialiased" :hash
  2586408642)
 :void (antialiased bool))

(defgmethod
 (line-2d+get-antialiased :class 'line-2d :bind "get_antialiased" :hash
  36873697)
 bool)