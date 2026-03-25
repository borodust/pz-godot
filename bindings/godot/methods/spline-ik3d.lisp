(common-lisp:in-package :%godot)


(defgmethod
 (spline-ik3d+set-path-3d :class 'spline-ik3d :bind "set_path_3d" :hash
  2761262315)
 :void (index int) (path-3d node-path))

(defgmethod
 (spline-ik3d+get-path-3d :class 'spline-ik3d :bind "get_path_3d" :hash
  408788394)
 node-path (index int))

(defgmethod
 (spline-ik3d+set-tilt-enabled :class 'spline-ik3d :bind "set_tilt_enabled"
  :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (spline-ik3d+is-tilt-enabled :class 'spline-ik3d :bind "is_tilt_enabled" :hash
  1116898809)
 bool (index int))

(defgmethod
 (spline-ik3d+set-tilt-fade-in :class 'spline-ik3d :bind "set_tilt_fade_in"
  :hash 3937882851)
 :void (index int) (size int))

(defgmethod
 (spline-ik3d+get-tilt-fade-in :class 'spline-ik3d :bind "get_tilt_fade_in"
  :hash 923996154)
 int (index int))

(defgmethod
 (spline-ik3d+set-tilt-fade-out :class 'spline-ik3d :bind "set_tilt_fade_out"
  :hash 3937882851)
 :void (index int) (size int))

(defgmethod
 (spline-ik3d+get-tilt-fade-out :class 'spline-ik3d :bind "get_tilt_fade_out"
  :hash 923996154)
 int (index int))