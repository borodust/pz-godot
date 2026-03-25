(common-lisp:in-package :%godot)


(defgproperty xrserver+world-scale 'xrserver :get 'xrserver+get-world-scale
 :set 'xrserver+set-world-scale)

(defgproperty xrserver+world-origin 'xrserver :get 'xrserver+get-world-origin
 :set 'xrserver+set-world-origin)

(defgproperty xrserver+camera-locked-to-origin 'xrserver :get
 'xrserver+is-camera-locked-to-origin :set
 'xrserver+set-camera-locked-to-origin)

(defgproperty xrserver+primary-interface 'xrserver :get
 'xrserver+get-primary-interface :set 'xrserver+set-primary-interface)