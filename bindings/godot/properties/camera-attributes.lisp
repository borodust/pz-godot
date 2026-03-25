(common-lisp:in-package :%godot)


(defgproperty camera-attributes+exposure-sensitivity 'camera-attributes :get
 'camera-attributes+get-exposure-sensitivity :set
 'camera-attributes+set-exposure-sensitivity)

(defgproperty camera-attributes+exposure-multiplier 'camera-attributes :get
 'camera-attributes+get-exposure-multiplier :set
 'camera-attributes+set-exposure-multiplier)

(defgproperty camera-attributes+auto-exposure-enabled 'camera-attributes :get
 'camera-attributes+is-auto-exposure-enabled :set
 'camera-attributes+set-auto-exposure-enabled)

(defgproperty camera-attributes+auto-exposure-scale 'camera-attributes :get
 'camera-attributes+get-auto-exposure-scale :set
 'camera-attributes+set-auto-exposure-scale)

(defgproperty camera-attributes+auto-exposure-speed 'camera-attributes :get
 'camera-attributes+get-auto-exposure-speed :set
 'camera-attributes+set-auto-exposure-speed)