(common-lisp:in-package :%godot)


(defgproperty camera-attributes-physical+frustum-focus-distance
 'camera-attributes-physical :get
 'camera-attributes-physical+get-focus-distance :set
 'camera-attributes-physical+set-focus-distance)

(defgproperty camera-attributes-physical+frustum-focal-length
 'camera-attributes-physical :get 'camera-attributes-physical+get-focal-length
 :set 'camera-attributes-physical+set-focal-length)

(defgproperty camera-attributes-physical+frustum-near
 'camera-attributes-physical :get 'camera-attributes-physical+get-near :set
 'camera-attributes-physical+set-near)

(defgproperty camera-attributes-physical+frustum-far
 'camera-attributes-physical :get 'camera-attributes-physical+get-far :set
 'camera-attributes-physical+set-far)

(defgproperty camera-attributes-physical+exposure-aperture
 'camera-attributes-physical :get 'camera-attributes-physical+get-aperture :set
 'camera-attributes-physical+set-aperture)

(defgproperty camera-attributes-physical+exposure-shutter-speed
 'camera-attributes-physical :get 'camera-attributes-physical+get-shutter-speed
 :set 'camera-attributes-physical+set-shutter-speed)

(defgproperty camera-attributes-physical+auto-exposure-min-exposure-value
 'camera-attributes-physical :get
 'camera-attributes-physical+get-auto-exposure-min-exposure-value :set
 'camera-attributes-physical+set-auto-exposure-min-exposure-value)

(defgproperty camera-attributes-physical+auto-exposure-max-exposure-value
 'camera-attributes-physical :get
 'camera-attributes-physical+get-auto-exposure-max-exposure-value :set
 'camera-attributes-physical+set-auto-exposure-max-exposure-value)