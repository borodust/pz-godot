(common-lisp:in-package :%godot)


(defgproperty sprite-2d+texture 'sprite-2d :get 'sprite-2d+get-texture :set
 'sprite-2d+set-texture)

(defgproperty sprite-2d+centered 'sprite-2d :get 'sprite-2d+is-centered :set
 'sprite-2d+set-centered)

(defgproperty sprite-2d+offset 'sprite-2d :get 'sprite-2d+get-offset :set
 'sprite-2d+set-offset)

(defgproperty sprite-2d+flip-h 'sprite-2d :get 'sprite-2d+is-flipped-h :set
 'sprite-2d+set-flip-h)

(defgproperty sprite-2d+flip-v 'sprite-2d :get 'sprite-2d+is-flipped-v :set
 'sprite-2d+set-flip-v)

(defgproperty sprite-2d+hframes 'sprite-2d :get 'sprite-2d+get-hframes :set
 'sprite-2d+set-hframes)

(defgproperty sprite-2d+vframes 'sprite-2d :get 'sprite-2d+get-vframes :set
 'sprite-2d+set-vframes)

(defgproperty sprite-2d+frame 'sprite-2d :get 'sprite-2d+get-frame :set
 'sprite-2d+set-frame)

(defgproperty sprite-2d+frame-coords 'sprite-2d :get
 'sprite-2d+get-frame-coords :set 'sprite-2d+set-frame-coords)

(defgproperty sprite-2d+region-enabled 'sprite-2d :get
 'sprite-2d+is-region-enabled :set 'sprite-2d+set-region-enabled)

(defgproperty sprite-2d+region-rect 'sprite-2d :get 'sprite-2d+get-region-rect
 :set 'sprite-2d+set-region-rect)

(defgproperty sprite-2d+region-filter-clip-enabled 'sprite-2d :get
 'sprite-2d+is-region-filter-clip-enabled :set
 'sprite-2d+set-region-filter-clip-enabled)