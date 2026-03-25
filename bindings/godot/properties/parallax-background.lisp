(common-lisp:in-package :%godot)


(defgproperty parallax-background+scroll-offset 'parallax-background :get
 'parallax-background+get-scroll-offset :set
 'parallax-background+set-scroll-offset)

(defgproperty parallax-background+scroll-base-offset 'parallax-background :get
 'parallax-background+get-scroll-base-offset :set
 'parallax-background+set-scroll-base-offset)

(defgproperty parallax-background+scroll-base-scale 'parallax-background :get
 'parallax-background+get-scroll-base-scale :set
 'parallax-background+set-scroll-base-scale)

(defgproperty parallax-background+scroll-limit-begin 'parallax-background :get
 'parallax-background+get-limit-begin :set 'parallax-background+set-limit-begin)

(defgproperty parallax-background+scroll-limit-end 'parallax-background :get
 'parallax-background+get-limit-end :set 'parallax-background+set-limit-end)

(defgproperty parallax-background+scroll-ignore-camera-zoom
 'parallax-background :get 'parallax-background+is-ignore-camera-zoom :set
 'parallax-background+set-ignore-camera-zoom)