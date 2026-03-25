(common-lisp:in-package :%godot)


(defgmethod
 (camera-attributes-physical+set-aperture :class 'camera-attributes-physical
  :bind "set_aperture" :hash 373806689)
 :void (aperture float))

(defgmethod
 (camera-attributes-physical+get-aperture :class 'camera-attributes-physical
  :bind "get_aperture" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes-physical+set-shutter-speed :class
  'camera-attributes-physical :bind "set_shutter_speed" :hash 373806689)
 :void (shutter-speed float))

(defgmethod
 (camera-attributes-physical+get-shutter-speed :class
  'camera-attributes-physical :bind "get_shutter_speed" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes-physical+set-focal-length :class
  'camera-attributes-physical :bind "set_focal_length" :hash 373806689)
 :void (focal-length float))

(defgmethod
 (camera-attributes-physical+get-focal-length :class
  'camera-attributes-physical :bind "get_focal_length" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes-physical+set-focus-distance :class
  'camera-attributes-physical :bind "set_focus_distance" :hash 373806689)
 :void (focus-distance float))

(defgmethod
 (camera-attributes-physical+get-focus-distance :class
  'camera-attributes-physical :bind "get_focus_distance" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes-physical+set-near :class 'camera-attributes-physical :bind
  "set_near" :hash 373806689)
 :void (near float))

(defgmethod
 (camera-attributes-physical+get-near :class 'camera-attributes-physical :bind
  "get_near" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes-physical+set-far :class 'camera-attributes-physical :bind
  "set_far" :hash 373806689)
 :void (far float))

(defgmethod
 (camera-attributes-physical+get-far :class 'camera-attributes-physical :bind
  "get_far" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes-physical+get-fov :class 'camera-attributes-physical :bind
  "get_fov" :hash 1740695150)
 float)

(defgmethod
 (camera-attributes-physical+set-auto-exposure-max-exposure-value :class
  'camera-attributes-physical :bind "set_auto_exposure_max_exposure_value"
  :hash 373806689)
 :void (exposure-value-max float))

(defgmethod
 (camera-attributes-physical+get-auto-exposure-max-exposure-value :class
  'camera-attributes-physical :bind "get_auto_exposure_max_exposure_value"
  :hash 1740695150)
 float)

(defgmethod
 (camera-attributes-physical+set-auto-exposure-min-exposure-value :class
  'camera-attributes-physical :bind "set_auto_exposure_min_exposure_value"
  :hash 373806689)
 :void (exposure-value-min float))

(defgmethod
 (camera-attributes-physical+get-auto-exposure-min-exposure-value :class
  'camera-attributes-physical :bind "get_auto_exposure_min_exposure_value"
  :hash 1740695150)
 float)