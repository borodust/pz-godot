(common-lisp:in-package :%godot)


(defgproperty parallax-2d+scroll-scale 'parallax-2d :get
 'parallax-2d+get-scroll-scale :set 'parallax-2d+set-scroll-scale)

(defgproperty parallax-2d+scroll-offset 'parallax-2d :get
 'parallax-2d+get-scroll-offset :set 'parallax-2d+set-scroll-offset)

(defgproperty parallax-2d+repeat-size 'parallax-2d :get
 'parallax-2d+get-repeat-size :set 'parallax-2d+set-repeat-size)

(defgproperty parallax-2d+autoscroll 'parallax-2d :get
 'parallax-2d+get-autoscroll :set 'parallax-2d+set-autoscroll)

(defgproperty parallax-2d+repeat-times 'parallax-2d :get
 'parallax-2d+get-repeat-times :set 'parallax-2d+set-repeat-times)

(defgproperty parallax-2d+limit-begin 'parallax-2d :get
 'parallax-2d+get-limit-begin :set 'parallax-2d+set-limit-begin)

(defgproperty parallax-2d+limit-end 'parallax-2d :get
 'parallax-2d+get-limit-end :set 'parallax-2d+set-limit-end)

(defgproperty parallax-2d+follow-viewport 'parallax-2d :get
 'parallax-2d+get-follow-viewport :set 'parallax-2d+set-follow-viewport)

(defgproperty parallax-2d+ignore-camera-scroll 'parallax-2d :get
 'parallax-2d+is-ignore-camera-scroll :set
 'parallax-2d+set-ignore-camera-scroll)

(defgproperty parallax-2d+screen-offset 'parallax-2d :get
 'parallax-2d+get-screen-offset :set 'parallax-2d+set-screen-offset)