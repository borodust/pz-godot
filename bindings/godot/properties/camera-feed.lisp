(common-lisp:in-package :%godot)


(defgproperty camera-feed+feed-is-active 'camera-feed :get
 'camera-feed+is-active :set 'camera-feed+set-active)

(defgproperty camera-feed+feed-transform 'camera-feed :get
 'camera-feed+get-transform :set 'camera-feed+set-transform)

(defgproperty camera-feed+formats 'camera-feed :get 'camera-feed+get-formats)