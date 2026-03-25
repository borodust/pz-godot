(common-lisp:in-package :%godot)


(defgmethod
 (curve+get-point-count :class 'curve :bind "get_point_count" :hash 3905245786)
 int)

(defgmethod
 (curve+set-point-count :class 'curve :bind "set_point_count" :hash 1286410249)
 :void (count int))

(defgmethod (curve+add-point :class 'curve :bind "add_point" :hash 434072736)
 int (position vector-2) (left-tangent float) (right-tangent float)
 (left-mode curve+tangent-mode) (right-mode curve+tangent-mode))

(defgmethod
 (curve+remove-point :class 'curve :bind "remove_point" :hash 1286410249) :void
 (index int))

(defgmethod
 (curve+clear-points :class 'curve :bind "clear_points" :hash 3218959716) :void)

(defgmethod
 (curve+get-point-position :class 'curve :bind "get_point_position" :hash
  2299179447)
 vector-2 (index int))

(defgmethod
 (curve+set-point-value :class 'curve :bind "set_point_value" :hash 1602489585)
 :void (index int) (y float))

(defgmethod
 (curve+set-point-offset :class 'curve :bind "set_point_offset" :hash
  3780573764)
 int (index int) (offset float))

(defgmethod (curve+sample :class 'curve :bind "sample" :hash 3919130443) float
 (offset float))

(defgmethod
 (curve+sample-baked :class 'curve :bind "sample_baked" :hash 3919130443) float
 (offset float))

(defgmethod
 (curve+get-point-left-tangent :class 'curve :bind "get_point_left_tangent"
  :hash 2339986948)
 float (index int))

(defgmethod
 (curve+get-point-right-tangent :class 'curve :bind "get_point_right_tangent"
  :hash 2339986948)
 float (index int))

(defgmethod
 (curve+get-point-left-mode :class 'curve :bind "get_point_left_mode" :hash
  426950354)
 curve+tangent-mode (index int))

(defgmethod
 (curve+get-point-right-mode :class 'curve :bind "get_point_right_mode" :hash
  426950354)
 curve+tangent-mode (index int))

(defgmethod
 (curve+set-point-left-tangent :class 'curve :bind "set_point_left_tangent"
  :hash 1602489585)
 :void (index int) (tangent float))

(defgmethod
 (curve+set-point-right-tangent :class 'curve :bind "set_point_right_tangent"
  :hash 1602489585)
 :void (index int) (tangent float))

(defgmethod
 (curve+set-point-left-mode :class 'curve :bind "set_point_left_mode" :hash
  1217242874)
 :void (index int) (mode curve+tangent-mode))

(defgmethod
 (curve+set-point-right-mode :class 'curve :bind "set_point_right_mode" :hash
  1217242874)
 :void (index int) (mode curve+tangent-mode))

(defgmethod
 (curve+get-min-value :class 'curve :bind "get_min_value" :hash 1740695150)
 float)

(defgmethod
 (curve+set-min-value :class 'curve :bind "set_min_value" :hash 373806689)
 :void (min float))

(defgmethod
 (curve+get-max-value :class 'curve :bind "get_max_value" :hash 1740695150)
 float)

(defgmethod
 (curve+set-max-value :class 'curve :bind "set_max_value" :hash 373806689)
 :void (max float))

(defgmethod
 (curve+get-value-range :class 'curve :bind "get_value_range" :hash 1740695150)
 float)

(defgmethod
 (curve+get-min-domain :class 'curve :bind "get_min_domain" :hash 1740695150)
 float)

(defgmethod
 (curve+set-min-domain :class 'curve :bind "set_min_domain" :hash 373806689)
 :void (min float))

(defgmethod
 (curve+get-max-domain :class 'curve :bind "get_max_domain" :hash 1740695150)
 float)

(defgmethod
 (curve+set-max-domain :class 'curve :bind "set_max_domain" :hash 373806689)
 :void (max float))

(defgmethod
 (curve+get-domain-range :class 'curve :bind "get_domain_range" :hash
  1740695150)
 float)

(defgmethod
 (curve+clean-dupes :class 'curve :bind "clean_dupes" :hash 3218959716) :void)

(defgmethod (curve+bake :class 'curve :bind "bake" :hash 3218959716) :void)

(defgmethod
 (curve+get-bake-resolution :class 'curve :bind "get_bake_resolution" :hash
  3905245786)
 int)

(defgmethod
 (curve+set-bake-resolution :class 'curve :bind "set_bake_resolution" :hash
  1286410249)
 :void (resolution int))