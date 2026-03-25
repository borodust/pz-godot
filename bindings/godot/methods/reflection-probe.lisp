(common-lisp:in-package :%godot)


(defgmethod
 (reflection-probe+set-intensity :class 'reflection-probe :bind "set_intensity"
  :hash 373806689)
 :void (intensity float))

(defgmethod
 (reflection-probe+get-intensity :class 'reflection-probe :bind "get_intensity"
  :hash 1740695150)
 float)

(defgmethod
 (reflection-probe+set-blend-distance :class 'reflection-probe :bind
  "set_blend_distance" :hash 373806689)
 :void (blend-distance float))

(defgmethod
 (reflection-probe+get-blend-distance :class 'reflection-probe :bind
  "get_blend_distance" :hash 1740695150)
 float)

(defgmethod
 (reflection-probe+set-ambient-mode :class 'reflection-probe :bind
  "set_ambient_mode" :hash 1748981278)
 :void (ambient reflection-probe+ambient-mode))

(defgmethod
 (reflection-probe+get-ambient-mode :class 'reflection-probe :bind
  "get_ambient_mode" :hash 1014607621)
 reflection-probe+ambient-mode)

(defgmethod
 (reflection-probe+set-ambient-color :class 'reflection-probe :bind
  "set_ambient_color" :hash 2920490490)
 :void (ambient color))

(defgmethod
 (reflection-probe+get-ambient-color :class 'reflection-probe :bind
  "get_ambient_color" :hash 3444240500)
 color)

(defgmethod
 (reflection-probe+set-ambient-color-energy :class 'reflection-probe :bind
  "set_ambient_color_energy" :hash 373806689)
 :void (ambient-energy float))

(defgmethod
 (reflection-probe+get-ambient-color-energy :class 'reflection-probe :bind
  "get_ambient_color_energy" :hash 1740695150)
 float)

(defgmethod
 (reflection-probe+set-max-distance :class 'reflection-probe :bind
  "set_max_distance" :hash 373806689)
 :void (max-distance float))

(defgmethod
 (reflection-probe+get-max-distance :class 'reflection-probe :bind
  "get_max_distance" :hash 1740695150)
 float)

(defgmethod
 (reflection-probe+set-mesh-lod-threshold :class 'reflection-probe :bind
  "set_mesh_lod_threshold" :hash 373806689)
 :void (ratio float))

(defgmethod
 (reflection-probe+get-mesh-lod-threshold :class 'reflection-probe :bind
  "get_mesh_lod_threshold" :hash 1740695150)
 float)

(defgmethod
 (reflection-probe+set-size :class 'reflection-probe :bind "set_size" :hash
  3460891852)
 :void (size vector-3))

(defgmethod
 (reflection-probe+get-size :class 'reflection-probe :bind "get_size" :hash
  3360562783)
 vector-3)

(defgmethod
 (reflection-probe+set-origin-offset :class 'reflection-probe :bind
  "set_origin_offset" :hash 3460891852)
 :void (origin-offset vector-3))

(defgmethod
 (reflection-probe+get-origin-offset :class 'reflection-probe :bind
  "get_origin_offset" :hash 3360562783)
 vector-3)

(defgmethod
 (reflection-probe+set-as-interior :class 'reflection-probe :bind
  "set_as_interior" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (reflection-probe+is-set-as-interior :class 'reflection-probe :bind
  "is_set_as_interior" :hash 36873697)
 bool)

(defgmethod
 (reflection-probe+set-enable-box-projection :class 'reflection-probe :bind
  "set_enable_box_projection" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (reflection-probe+is-box-projection-enabled :class 'reflection-probe :bind
  "is_box_projection_enabled" :hash 36873697)
 bool)

(defgmethod
 (reflection-probe+set-enable-shadows :class 'reflection-probe :bind
  "set_enable_shadows" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (reflection-probe+are-shadows-enabled :class 'reflection-probe :bind
  "are_shadows_enabled" :hash 36873697)
 bool)

(defgmethod
 (reflection-probe+set-cull-mask :class 'reflection-probe :bind "set_cull_mask"
  :hash 1286410249)
 :void (layers int))

(defgmethod
 (reflection-probe+get-cull-mask :class 'reflection-probe :bind "get_cull_mask"
  :hash 3905245786)
 int)

(defgmethod
 (reflection-probe+set-reflection-mask :class 'reflection-probe :bind
  "set_reflection_mask" :hash 1286410249)
 :void (layers int))

(defgmethod
 (reflection-probe+get-reflection-mask :class 'reflection-probe :bind
  "get_reflection_mask" :hash 3905245786)
 int)

(defgmethod
 (reflection-probe+set-update-mode :class 'reflection-probe :bind
  "set_update_mode" :hash 4090221187)
 :void (mode reflection-probe+update-mode))

(defgmethod
 (reflection-probe+get-update-mode :class 'reflection-probe :bind
  "get_update_mode" :hash 2367550552)
 reflection-probe+update-mode)