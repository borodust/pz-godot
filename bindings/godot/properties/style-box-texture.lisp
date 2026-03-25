(common-lisp:in-package :%godot)


(defgproperty style-box-texture+texture 'style-box-texture :get
 'style-box-texture+get-texture :set 'style-box-texture+set-texture)

(defgproperty style-box-texture+texture-margin-left 'style-box-texture :index 0
 :get 'style-box-texture+get-texture-margin :set
 'style-box-texture+set-texture-margin)

(defgproperty style-box-texture+texture-margin-top 'style-box-texture :index 1
 :get 'style-box-texture+get-texture-margin :set
 'style-box-texture+set-texture-margin)

(defgproperty style-box-texture+texture-margin-right 'style-box-texture :index
 2 :get 'style-box-texture+get-texture-margin :set
 'style-box-texture+set-texture-margin)

(defgproperty style-box-texture+texture-margin-bottom 'style-box-texture :index
 3 :get 'style-box-texture+get-texture-margin :set
 'style-box-texture+set-texture-margin)

(defgproperty style-box-texture+expand-margin-left 'style-box-texture :index 0
 :get 'style-box-texture+get-expand-margin :set
 'style-box-texture+set-expand-margin)

(defgproperty style-box-texture+expand-margin-top 'style-box-texture :index 1
 :get 'style-box-texture+get-expand-margin :set
 'style-box-texture+set-expand-margin)

(defgproperty style-box-texture+expand-margin-right 'style-box-texture :index 2
 :get 'style-box-texture+get-expand-margin :set
 'style-box-texture+set-expand-margin)

(defgproperty style-box-texture+expand-margin-bottom 'style-box-texture :index
 3 :get 'style-box-texture+get-expand-margin :set
 'style-box-texture+set-expand-margin)

(defgproperty style-box-texture+axis-stretch-horizontal 'style-box-texture :get
 'style-box-texture+get-h-axis-stretch-mode :set
 'style-box-texture+set-h-axis-stretch-mode)

(defgproperty style-box-texture+axis-stretch-vertical 'style-box-texture :get
 'style-box-texture+get-v-axis-stretch-mode :set
 'style-box-texture+set-v-axis-stretch-mode)

(defgproperty style-box-texture+region-rect 'style-box-texture :get
 'style-box-texture+get-region-rect :set 'style-box-texture+set-region-rect)

(defgproperty style-box-texture+modulate-color 'style-box-texture :get
 'style-box-texture+get-modulate :set 'style-box-texture+set-modulate)

(defgproperty style-box-texture+draw-center 'style-box-texture :get
 'style-box-texture+is-draw-center-enabled :set
 'style-box-texture+set-draw-center)