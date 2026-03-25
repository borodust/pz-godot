(common-lisp:in-package :%godot)


(defgmethod
 (camera-attributes-practical+set-dof-blur-far-enabled :class
  'camera-attributes-practical :bind "set_dof_blur_far_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (camera-attributes-practical+is-dof-blur-far-enabled :class
  'camera-attributes-practical :bind "is_dof_blur_far_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-attributes-practical+set-dof-blur-far-distance :class
  'camera-attributes-practical :bind "set_dof_blur_far_distance" :hash
  373806689)
 :void (distance float))

(defgmethod
 (camera-attributes-practical+get-dof-blur-far-distance :class
  'camera-attributes-practical :bind "get_dof_blur_far_distance" :hash
  1740695150)
 float)

(defgmethod
 (camera-attributes-practical+set-dof-blur-far-transition :class
  'camera-attributes-practical :bind "set_dof_blur_far_transition" :hash
  373806689)
 :void (distance float))

(defgmethod
 (camera-attributes-practical+get-dof-blur-far-transition :class
  'camera-attributes-practical :bind "get_dof_blur_far_transition" :hash
  1740695150)
 float)

(defgmethod
 (camera-attributes-practical+set-dof-blur-near-enabled :class
  'camera-attributes-practical :bind "set_dof_blur_near_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (camera-attributes-practical+is-dof-blur-near-enabled :class
  'camera-attributes-practical :bind "is_dof_blur_near_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-attributes-practical+set-dof-blur-near-distance :class
  'camera-attributes-practical :bind "set_dof_blur_near_distance" :hash
  373806689)
 :void (distance float))

(defgmethod
 (camera-attributes-practical+get-dof-blur-near-distance :class
  'camera-attributes-practical :bind "get_dof_blur_near_distance" :hash
  1740695150)
 float)

(defgmethod
 (camera-attributes-practical+set-dof-blur-near-transition :class
  'camera-attributes-practical :bind "set_dof_blur_near_transition" :hash
  373806689)
 :void (distance float))

(defgmethod
 (camera-attributes-practical+get-dof-blur-near-transition :class
  'camera-attributes-practical :bind "get_dof_blur_near_transition" :hash
  1740695150)
 float)

(defgmethod
 (camera-attributes-practical+set-dof-blur-amount :class
  'camera-attributes-practical :bind "set_dof_blur_amount" :hash 373806689)
 :void (amount float))

(defgmethod
 (camera-attributes-practical+get-dof-blur-amount :class
  'camera-attributes-practical :bind "get_dof_blur_amount" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes-practical+set-auto-exposure-max-sensitivity :class
  'camera-attributes-practical :bind "set_auto_exposure_max_sensitivity" :hash
  373806689)
 :void (max-sensitivity float))

(defgmethod
 (camera-attributes-practical+get-auto-exposure-max-sensitivity :class
  'camera-attributes-practical :bind "get_auto_exposure_max_sensitivity" :hash
  1740695150)
 float)

(defgmethod
 (camera-attributes-practical+set-auto-exposure-min-sensitivity :class
  'camera-attributes-practical :bind "set_auto_exposure_min_sensitivity" :hash
  373806689)
 :void (min-sensitivity float))

(defgmethod
 (camera-attributes-practical+get-auto-exposure-min-sensitivity :class
  'camera-attributes-practical :bind "get_auto_exposure_min_sensitivity" :hash
  1740695150)
 float)