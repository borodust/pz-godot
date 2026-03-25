(common-lisp:in-package :%godot)


(defgproperty nine-patch-rect+texture 'nine-patch-rect :get
 'nine-patch-rect+get-texture :set 'nine-patch-rect+set-texture)

(defgproperty nine-patch-rect+draw-center 'nine-patch-rect :get
 'nine-patch-rect+is-draw-center-enabled :set 'nine-patch-rect+set-draw-center)

(defgproperty nine-patch-rect+region-rect 'nine-patch-rect :get
 'nine-patch-rect+get-region-rect :set 'nine-patch-rect+set-region-rect)

(defgproperty nine-patch-rect+patch-margin-left 'nine-patch-rect :index 0 :get
 'nine-patch-rect+get-patch-margin :set 'nine-patch-rect+set-patch-margin)

(defgproperty nine-patch-rect+patch-margin-top 'nine-patch-rect :index 1 :get
 'nine-patch-rect+get-patch-margin :set 'nine-patch-rect+set-patch-margin)

(defgproperty nine-patch-rect+patch-margin-right 'nine-patch-rect :index 2 :get
 'nine-patch-rect+get-patch-margin :set 'nine-patch-rect+set-patch-margin)

(defgproperty nine-patch-rect+patch-margin-bottom 'nine-patch-rect :index 3
 :get 'nine-patch-rect+get-patch-margin :set 'nine-patch-rect+set-patch-margin)

(defgproperty nine-patch-rect+axis-stretch-horizontal 'nine-patch-rect :get
 'nine-patch-rect+get-h-axis-stretch-mode :set
 'nine-patch-rect+set-h-axis-stretch-mode)

(defgproperty nine-patch-rect+axis-stretch-vertical 'nine-patch-rect :get
 'nine-patch-rect+get-v-axis-stretch-mode :set
 'nine-patch-rect+set-v-axis-stretch-mode)