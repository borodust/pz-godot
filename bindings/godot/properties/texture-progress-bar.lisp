(common-lisp:in-package :%godot)


(defgproperty texture-progress-bar+fill-mode 'texture-progress-bar :get
 'texture-progress-bar+get-fill-mode :set 'texture-progress-bar+set-fill-mode)

(defgproperty texture-progress-bar+radial-initial-angle 'texture-progress-bar
 :get 'texture-progress-bar+get-radial-initial-angle :set
 'texture-progress-bar+set-radial-initial-angle)

(defgproperty texture-progress-bar+radial-fill-degrees 'texture-progress-bar
 :get 'texture-progress-bar+get-fill-degrees :set
 'texture-progress-bar+set-fill-degrees)

(defgproperty texture-progress-bar+radial-center-offset 'texture-progress-bar
 :get 'texture-progress-bar+get-radial-center-offset :set
 'texture-progress-bar+set-radial-center-offset)

(defgproperty texture-progress-bar+nine-patch-stretch 'texture-progress-bar
 :get 'texture-progress-bar+get-nine-patch-stretch :set
 'texture-progress-bar+set-nine-patch-stretch)

(defgproperty texture-progress-bar+stretch-margin-left 'texture-progress-bar
 :index 0 :get 'texture-progress-bar+get-stretch-margin :set
 'texture-progress-bar+set-stretch-margin)

(defgproperty texture-progress-bar+stretch-margin-top 'texture-progress-bar
 :index 1 :get 'texture-progress-bar+get-stretch-margin :set
 'texture-progress-bar+set-stretch-margin)

(defgproperty texture-progress-bar+stretch-margin-right 'texture-progress-bar
 :index 2 :get 'texture-progress-bar+get-stretch-margin :set
 'texture-progress-bar+set-stretch-margin)

(defgproperty texture-progress-bar+stretch-margin-bottom 'texture-progress-bar
 :index 3 :get 'texture-progress-bar+get-stretch-margin :set
 'texture-progress-bar+set-stretch-margin)

(defgproperty texture-progress-bar+texture-under 'texture-progress-bar :get
 'texture-progress-bar+get-under-texture :set
 'texture-progress-bar+set-under-texture)

(defgproperty texture-progress-bar+texture-over 'texture-progress-bar :get
 'texture-progress-bar+get-over-texture :set
 'texture-progress-bar+set-over-texture)

(defgproperty texture-progress-bar+texture-progress 'texture-progress-bar :get
 'texture-progress-bar+get-progress-texture :set
 'texture-progress-bar+set-progress-texture)

(defgproperty texture-progress-bar+texture-progress-offset
 'texture-progress-bar :get 'texture-progress-bar+get-texture-progress-offset
 :set 'texture-progress-bar+set-texture-progress-offset)

(defgproperty texture-progress-bar+tint-under 'texture-progress-bar :get
 'texture-progress-bar+get-tint-under :set 'texture-progress-bar+set-tint-under)

(defgproperty texture-progress-bar+tint-over 'texture-progress-bar :get
 'texture-progress-bar+get-tint-over :set 'texture-progress-bar+set-tint-over)

(defgproperty texture-progress-bar+tint-progress 'texture-progress-bar :get
 'texture-progress-bar+get-tint-progress :set
 'texture-progress-bar+set-tint-progress)