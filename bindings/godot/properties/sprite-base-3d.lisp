(common-lisp:in-package :%godot)


(defgproperty sprite-base-3d+centered 'sprite-base-3d :get
 'sprite-base-3d+is-centered :set 'sprite-base-3d+set-centered)

(defgproperty sprite-base-3d+offset 'sprite-base-3d :get
 'sprite-base-3d+get-offset :set 'sprite-base-3d+set-offset)

(defgproperty sprite-base-3d+flip-h 'sprite-base-3d :get
 'sprite-base-3d+is-flipped-h :set 'sprite-base-3d+set-flip-h)

(defgproperty sprite-base-3d+flip-v 'sprite-base-3d :get
 'sprite-base-3d+is-flipped-v :set 'sprite-base-3d+set-flip-v)

(defgproperty sprite-base-3d+modulate 'sprite-base-3d :get
 'sprite-base-3d+get-modulate :set 'sprite-base-3d+set-modulate)

(defgproperty sprite-base-3d+pixel-size 'sprite-base-3d :get
 'sprite-base-3d+get-pixel-size :set 'sprite-base-3d+set-pixel-size)

(defgproperty sprite-base-3d+axis 'sprite-base-3d :get 'sprite-base-3d+get-axis
 :set 'sprite-base-3d+set-axis)

(defgproperty sprite-base-3d+billboard 'sprite-base-3d :get
 'sprite-base-3d+get-billboard-mode :set 'sprite-base-3d+set-billboard-mode)

(defgproperty sprite-base-3d+transparent 'sprite-base-3d :index 0 :get
 'sprite-base-3d+get-draw-flag :set 'sprite-base-3d+set-draw-flag)

(defgproperty sprite-base-3d+shaded 'sprite-base-3d :index 1 :get
 'sprite-base-3d+get-draw-flag :set 'sprite-base-3d+set-draw-flag)

(defgproperty sprite-base-3d+double-sided 'sprite-base-3d :index 2 :get
 'sprite-base-3d+get-draw-flag :set 'sprite-base-3d+set-draw-flag)

(defgproperty sprite-base-3d+no-depth-test 'sprite-base-3d :index 3 :get
 'sprite-base-3d+get-draw-flag :set 'sprite-base-3d+set-draw-flag)

(defgproperty sprite-base-3d+fixed-size 'sprite-base-3d :index 4 :get
 'sprite-base-3d+get-draw-flag :set 'sprite-base-3d+set-draw-flag)

(defgproperty sprite-base-3d+alpha-cut 'sprite-base-3d :get
 'sprite-base-3d+get-alpha-cut-mode :set 'sprite-base-3d+set-alpha-cut-mode)

(defgproperty sprite-base-3d+alpha-scissor-threshold 'sprite-base-3d :get
 'sprite-base-3d+get-alpha-scissor-threshold :set
 'sprite-base-3d+set-alpha-scissor-threshold)

(defgproperty sprite-base-3d+alpha-hash-scale 'sprite-base-3d :get
 'sprite-base-3d+get-alpha-hash-scale :set 'sprite-base-3d+set-alpha-hash-scale)

(defgproperty sprite-base-3d+alpha-antialiasing-mode 'sprite-base-3d :get
 'sprite-base-3d+get-alpha-antialiasing :set
 'sprite-base-3d+set-alpha-antialiasing)

(defgproperty sprite-base-3d+alpha-antialiasing-edge 'sprite-base-3d :get
 'sprite-base-3d+get-alpha-antialiasing-edge :set
 'sprite-base-3d+set-alpha-antialiasing-edge)

(defgproperty sprite-base-3d+texture-filter 'sprite-base-3d :get
 'sprite-base-3d+get-texture-filter :set 'sprite-base-3d+set-texture-filter)

(defgproperty sprite-base-3d+render-priority 'sprite-base-3d :get
 'sprite-base-3d+get-render-priority :set 'sprite-base-3d+set-render-priority)