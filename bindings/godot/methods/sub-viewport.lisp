(common-lisp:in-package :%godot)


(defgmethod
 (sub-viewport+set-size :class 'sub-viewport :bind "set_size" :hash 1130785943)
 :void (size vector-2i))

(defgmethod
 (sub-viewport+get-size :class 'sub-viewport :bind "get_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (sub-viewport+set-size-2d-override :class 'sub-viewport :bind
  "set_size_2d_override" :hash 1130785943)
 :void (size vector-2i))

(defgmethod
 (sub-viewport+get-size-2d-override :class 'sub-viewport :bind
  "get_size_2d_override" :hash 3690982128)
 vector-2i)

(defgmethod
 (sub-viewport+set-size-2d-override-stretch :class 'sub-viewport :bind
  "set_size_2d_override_stretch" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (sub-viewport+is-size-2d-override-stretch-enabled :class 'sub-viewport :bind
  "is_size_2d_override_stretch_enabled" :hash 36873697)
 bool)

(defgmethod
 (sub-viewport+set-update-mode :class 'sub-viewport :bind "set_update_mode"
  :hash 1295690030)
 :void (mode sub-viewport+update-mode))

(defgmethod
 (sub-viewport+get-update-mode :class 'sub-viewport :bind "get_update_mode"
  :hash 2980171553)
 sub-viewport+update-mode)

(defgmethod
 (sub-viewport+set-clear-mode :class 'sub-viewport :bind "set_clear_mode" :hash
  2834454712)
 :void (mode sub-viewport+clear-mode))

(defgmethod
 (sub-viewport+get-clear-mode :class 'sub-viewport :bind "get_clear_mode" :hash
  331324495)
 sub-viewport+clear-mode)