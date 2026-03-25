(common-lisp:in-package :%godot)


(defgmethod
 (multi-mesh+set-mesh :class 'multi-mesh :bind "set_mesh" :hash 194775623)
 :void (mesh mesh))

(defgmethod
 (multi-mesh+get-mesh :class 'multi-mesh :bind "get_mesh" :hash 1808005922)
 mesh)

(defgmethod
 (multi-mesh+set-use-colors :class 'multi-mesh :bind "set_use_colors" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (multi-mesh+is-using-colors :class 'multi-mesh :bind "is_using_colors" :hash
  36873697)
 bool)

(defgmethod
 (multi-mesh+set-use-custom-data :class 'multi-mesh :bind "set_use_custom_data"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (multi-mesh+is-using-custom-data :class 'multi-mesh :bind
  "is_using_custom_data" :hash 36873697)
 bool)

(defgmethod
 (multi-mesh+set-transform-format :class 'multi-mesh :bind
  "set_transform_format" :hash 2404750322)
 :void (format multi-mesh+transform-format))

(defgmethod
 (multi-mesh+get-transform-format :class 'multi-mesh :bind
  "get_transform_format" :hash 2444156481)
 multi-mesh+transform-format)

(defgmethod
 (multi-mesh+set-instance-count :class 'multi-mesh :bind "set_instance_count"
  :hash 1286410249)
 :void (count int))

(defgmethod
 (multi-mesh+get-instance-count :class 'multi-mesh :bind "get_instance_count"
  :hash 3905245786)
 int)

(defgmethod
 (multi-mesh+set-visible-instance-count :class 'multi-mesh :bind
  "set_visible_instance_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (multi-mesh+get-visible-instance-count :class 'multi-mesh :bind
  "get_visible_instance_count" :hash 3905245786)
 int)

(defgmethod
 (multi-mesh+set-physics-interpolation-quality :class 'multi-mesh :bind
  "set_physics_interpolation_quality" :hash 1819488408)
 :void (quality multi-mesh+physics-interpolation-quality))

(defgmethod
 (multi-mesh+get-physics-interpolation-quality :class 'multi-mesh :bind
  "get_physics_interpolation_quality" :hash 1465701882)
 multi-mesh+physics-interpolation-quality)

(defgmethod
 (multi-mesh+set-instance-transform :class 'multi-mesh :bind
  "set_instance_transform" :hash 3616898986)
 :void (instance int) (transform transform-3d))

(defgmethod
 (multi-mesh+set-instance-transform-2d :class 'multi-mesh :bind
  "set_instance_transform_2d" :hash 30160968)
 :void (instance int) (transform transform-2d))

(defgmethod
 (multi-mesh+get-instance-transform :class 'multi-mesh :bind
  "get_instance_transform" :hash 1965739696)
 transform-3d (instance int))

(defgmethod
 (multi-mesh+get-instance-transform-2d :class 'multi-mesh :bind
  "get_instance_transform_2d" :hash 3836996910)
 transform-2d (instance int))

(defgmethod
 (multi-mesh+set-instance-color :class 'multi-mesh :bind "set_instance_color"
  :hash 2878471219)
 :void (instance int) (color color))

(defgmethod
 (multi-mesh+get-instance-color :class 'multi-mesh :bind "get_instance_color"
  :hash 3457211756)
 color (instance int))

(defgmethod
 (multi-mesh+set-instance-custom-data :class 'multi-mesh :bind
  "set_instance_custom_data" :hash 2878471219)
 :void (instance int) (custom-data color))

(defgmethod
 (multi-mesh+get-instance-custom-data :class 'multi-mesh :bind
  "get_instance_custom_data" :hash 3457211756)
 color (instance int))

(defgmethod
 (multi-mesh+reset-instance-physics-interpolation :class 'multi-mesh :bind
  "reset_instance_physics_interpolation" :hash 1286410249)
 :void (instance int))

(defgmethod
 (multi-mesh+reset-instances-physics-interpolation :class 'multi-mesh :bind
  "reset_instances_physics_interpolation" :hash 3218959716)
 :void)

(defgmethod
 (multi-mesh+set-custom-aabb :class 'multi-mesh :bind "set_custom_aabb" :hash
  259215842)
 :void (aabb aabb))

(defgmethod
 (multi-mesh+get-custom-aabb :class 'multi-mesh :bind "get_custom_aabb" :hash
  1068685055)
 aabb)

(defgmethod
 (multi-mesh+get-aabb :class 'multi-mesh :bind "get_aabb" :hash 1068685055)
 aabb)

(defgmethod
 (multi-mesh+get-buffer :class 'multi-mesh :bind "get_buffer" :hash 675695659)
 packed-float-32array)

(defgmethod
 (multi-mesh+set-buffer :class 'multi-mesh :bind "set_buffer" :hash 2899603908)
 :void (buffer packed-float-32array))

(defgmethod
 (multi-mesh+set-buffer-interpolated :class 'multi-mesh :bind
  "set_buffer_interpolated" :hash 3514430332)
 :void (buffer-curr packed-float-32array) (buffer-prev packed-float-32array))