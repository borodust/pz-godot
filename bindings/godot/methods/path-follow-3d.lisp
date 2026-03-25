(common-lisp:in-package :%godot)


(defgmethod
 (path-follow-3d+set-progress :class 'path-follow-3d :bind "set_progress" :hash
  373806689)
 :void (progress float))

(defgmethod
 (path-follow-3d+get-progress :class 'path-follow-3d :bind "get_progress" :hash
  1740695150)
 float)

(defgmethod
 (path-follow-3d+set-h-offset :class 'path-follow-3d :bind "set_h_offset" :hash
  373806689)
 :void (h-offset float))

(defgmethod
 (path-follow-3d+get-h-offset :class 'path-follow-3d :bind "get_h_offset" :hash
  1740695150)
 float)

(defgmethod
 (path-follow-3d+set-v-offset :class 'path-follow-3d :bind "set_v_offset" :hash
  373806689)
 :void (v-offset float))

(defgmethod
 (path-follow-3d+get-v-offset :class 'path-follow-3d :bind "get_v_offset" :hash
  1740695150)
 float)

(defgmethod
 (path-follow-3d+set-progress-ratio :class 'path-follow-3d :bind
  "set_progress_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (path-follow-3d+get-progress-ratio :class 'path-follow-3d :bind
  "get_progress_ratio" :hash 1740695150)
 float)

(defgmethod
 (path-follow-3d+set-rotation-mode :class 'path-follow-3d :bind
  "set_rotation_mode" :hash 1640311967)
 :void (rotation-mode path-follow-3d+rotation-mode))

(defgmethod
 (path-follow-3d+get-rotation-mode :class 'path-follow-3d :bind
  "get_rotation_mode" :hash 3814010545)
 path-follow-3d+rotation-mode)

(defgmethod
 (path-follow-3d+set-cubic-interpolation :class 'path-follow-3d :bind
  "set_cubic_interpolation" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (path-follow-3d+get-cubic-interpolation :class 'path-follow-3d :bind
  "get_cubic_interpolation" :hash 36873697)
 bool)

(defgmethod
 (path-follow-3d+set-use-model-front :class 'path-follow-3d :bind
  "set_use_model_front" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (path-follow-3d+is-using-model-front :class 'path-follow-3d :bind
  "is_using_model_front" :hash 36873697)
 bool)

(defgmethod
 (path-follow-3d+set-loop :class 'path-follow-3d :bind "set_loop" :hash
  2586408642)
 :void (loop bool))

(defgmethod
 (path-follow-3d+has-loop :class 'path-follow-3d :bind "has_loop" :hash
  36873697)
 bool)

(defgmethod
 (path-follow-3d+set-tilt-enabled :class 'path-follow-3d :bind
  "set_tilt_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (path-follow-3d+is-tilt-enabled :class 'path-follow-3d :bind "is_tilt_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (path-follow-3d+correct-posture :class 'path-follow-3d :bind "correct_posture"
  :hash 2686588690 :static common-lisp:t)
 transform-3d (transform transform-3d)
 (rotation-mode path-follow-3d+rotation-mode))