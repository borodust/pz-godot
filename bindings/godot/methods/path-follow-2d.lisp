(common-lisp:in-package :%godot)


(defgmethod
 (path-follow-2d+set-progress :class 'path-follow-2d :bind "set_progress" :hash
  373806689)
 :void (progress float))

(defgmethod
 (path-follow-2d+get-progress :class 'path-follow-2d :bind "get_progress" :hash
  1740695150)
 float)

(defgmethod
 (path-follow-2d+set-h-offset :class 'path-follow-2d :bind "set_h_offset" :hash
  373806689)
 :void (h-offset float))

(defgmethod
 (path-follow-2d+get-h-offset :class 'path-follow-2d :bind "get_h_offset" :hash
  1740695150)
 float)

(defgmethod
 (path-follow-2d+set-v-offset :class 'path-follow-2d :bind "set_v_offset" :hash
  373806689)
 :void (v-offset float))

(defgmethod
 (path-follow-2d+get-v-offset :class 'path-follow-2d :bind "get_v_offset" :hash
  1740695150)
 float)

(defgmethod
 (path-follow-2d+set-progress-ratio :class 'path-follow-2d :bind
  "set_progress_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (path-follow-2d+get-progress-ratio :class 'path-follow-2d :bind
  "get_progress_ratio" :hash 1740695150)
 float)

(defgmethod
 (path-follow-2d+set-rotates :class 'path-follow-2d :bind "set_rotates" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (path-follow-2d+is-rotating :class 'path-follow-2d :bind "is_rotating" :hash
  36873697)
 bool)

(defgmethod
 (path-follow-2d+set-cubic-interpolation :class 'path-follow-2d :bind
  "set_cubic_interpolation" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (path-follow-2d+get-cubic-interpolation :class 'path-follow-2d :bind
  "get_cubic_interpolation" :hash 36873697)
 bool)

(defgmethod
 (path-follow-2d+set-loop :class 'path-follow-2d :bind "set_loop" :hash
  2586408642)
 :void (loop bool))

(defgmethod
 (path-follow-2d+has-loop :class 'path-follow-2d :bind "has_loop" :hash
  36873697)
 bool)