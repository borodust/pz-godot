(common-lisp:in-package :%godot)


(defgmethod
 (camera-attributes+set-exposure-multiplier :class 'camera-attributes :bind
  "set_exposure_multiplier" :hash 373806689)
 :void (multiplier float))

(defgmethod
 (camera-attributes+get-exposure-multiplier :class 'camera-attributes :bind
  "get_exposure_multiplier" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes+set-exposure-sensitivity :class 'camera-attributes :bind
  "set_exposure_sensitivity" :hash 373806689)
 :void (sensitivity float))

(defgmethod
 (camera-attributes+get-exposure-sensitivity :class 'camera-attributes :bind
  "get_exposure_sensitivity" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes+set-auto-exposure-enabled :class 'camera-attributes :bind
  "set_auto_exposure_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (camera-attributes+is-auto-exposure-enabled :class 'camera-attributes :bind
  "is_auto_exposure_enabled" :hash 36873697)
 bool)

(defgmethod
 (camera-attributes+set-auto-exposure-speed :class 'camera-attributes :bind
  "set_auto_exposure_speed" :hash 373806689)
 :void (exposure-speed float))

(defgmethod
 (camera-attributes+get-auto-exposure-speed :class 'camera-attributes :bind
  "get_auto_exposure_speed" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes+set-auto-exposure-scale :class 'camera-attributes :bind
  "set_auto_exposure_scale" :hash 373806689)
 :void (exposure-grey float))

(defgmethod
 (camera-attributes+get-auto-exposure-scale :class 'camera-attributes :bind
  "get_auto_exposure_scale" :hash 1740695150)
 float)