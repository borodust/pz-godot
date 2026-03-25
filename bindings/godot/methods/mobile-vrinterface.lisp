(common-lisp:in-package :%godot)


(defgmethod
 (mobile-vrinterface+set-eye-height :class 'mobile-vrinterface :bind
  "set_eye_height" :hash 373806689)
 :void (eye-height float))

(defgmethod
 (mobile-vrinterface+get-eye-height :class 'mobile-vrinterface :bind
  "get_eye_height" :hash 1740695150)
 float)

(defgmethod
 (mobile-vrinterface+set-iod :class 'mobile-vrinterface :bind "set_iod" :hash
  373806689)
 :void (iod float))

(defgmethod
 (mobile-vrinterface+get-iod :class 'mobile-vrinterface :bind "get_iod" :hash
  1740695150)
 float)

(defgmethod
 (mobile-vrinterface+set-display-width :class 'mobile-vrinterface :bind
  "set_display_width" :hash 373806689)
 :void (display-width float))

(defgmethod
 (mobile-vrinterface+get-display-width :class 'mobile-vrinterface :bind
  "get_display_width" :hash 1740695150)
 float)

(defgmethod
 (mobile-vrinterface+set-display-to-lens :class 'mobile-vrinterface :bind
  "set_display_to_lens" :hash 373806689)
 :void (display-to-lens float))

(defgmethod
 (mobile-vrinterface+get-display-to-lens :class 'mobile-vrinterface :bind
  "get_display_to_lens" :hash 1740695150)
 float)

(defgmethod
 (mobile-vrinterface+set-offset-rect :class 'mobile-vrinterface :bind
  "set_offset_rect" :hash 2046264180)
 :void (offset-rect rect-2))

(defgmethod
 (mobile-vrinterface+get-offset-rect :class 'mobile-vrinterface :bind
  "get_offset_rect" :hash 1639390495)
 rect-2)

(defgmethod
 (mobile-vrinterface+set-oversample :class 'mobile-vrinterface :bind
  "set_oversample" :hash 373806689)
 :void (oversample float))

(defgmethod
 (mobile-vrinterface+get-oversample :class 'mobile-vrinterface :bind
  "get_oversample" :hash 1740695150)
 float)

(defgmethod
 (mobile-vrinterface+set-k1 :class 'mobile-vrinterface :bind "set_k1" :hash
  373806689)
 :void (k float))

(defgmethod
 (mobile-vrinterface+get-k1 :class 'mobile-vrinterface :bind "get_k1" :hash
  1740695150)
 float)

(defgmethod
 (mobile-vrinterface+set-k2 :class 'mobile-vrinterface :bind "set_k2" :hash
  373806689)
 :void (k float))

(defgmethod
 (mobile-vrinterface+get-k2 :class 'mobile-vrinterface :bind "get_k2" :hash
  1740695150)
 float)

(defgmethod
 (mobile-vrinterface+get-vrs-min-radius :class 'mobile-vrinterface :bind
  "get_vrs_min_radius" :hash 1740695150)
 float)

(defgmethod
 (mobile-vrinterface+set-vrs-min-radius :class 'mobile-vrinterface :bind
  "set_vrs_min_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (mobile-vrinterface+get-vrs-strength :class 'mobile-vrinterface :bind
  "get_vrs_strength" :hash 1740695150)
 float)

(defgmethod
 (mobile-vrinterface+set-vrs-strength :class 'mobile-vrinterface :bind
  "set_vrs_strength" :hash 373806689)
 :void (strength float))