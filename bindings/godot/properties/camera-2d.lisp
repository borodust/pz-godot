(common-lisp:in-package :%godot)


(defgproperty camera-2d+offset 'camera-2d :get 'camera-2d+get-offset :set
 'camera-2d+set-offset)

(defgproperty camera-2d+anchor-mode 'camera-2d :get 'camera-2d+get-anchor-mode
 :set 'camera-2d+set-anchor-mode)

(defgproperty camera-2d+ignore-rotation 'camera-2d :get
 'camera-2d+is-ignoring-rotation :set 'camera-2d+set-ignore-rotation)

(defgproperty camera-2d+enabled 'camera-2d :get 'camera-2d+is-enabled :set
 'camera-2d+set-enabled)

(defgproperty camera-2d+zoom 'camera-2d :get 'camera-2d+get-zoom :set
 'camera-2d+set-zoom)

(defgproperty camera-2d+custom-viewport 'camera-2d :get
 'camera-2d+get-custom-viewport :set 'camera-2d+set-custom-viewport)

(defgproperty camera-2d+process-callback 'camera-2d :get
 'camera-2d+get-process-callback :set 'camera-2d+set-process-callback)

(defgproperty camera-2d+limit-enabled 'camera-2d :get
 'camera-2d+is-limit-enabled :set 'camera-2d+set-limit-enabled)

(defgproperty camera-2d+limit-left 'camera-2d :index 0 :get
 'camera-2d+get-limit :set 'camera-2d+set-limit)

(defgproperty camera-2d+limit-top 'camera-2d :index 1 :get 'camera-2d+get-limit
 :set 'camera-2d+set-limit)

(defgproperty camera-2d+limit-right 'camera-2d :index 2 :get
 'camera-2d+get-limit :set 'camera-2d+set-limit)

(defgproperty camera-2d+limit-bottom 'camera-2d :index 3 :get
 'camera-2d+get-limit :set 'camera-2d+set-limit)

(defgproperty camera-2d+limit-smoothed 'camera-2d :get
 'camera-2d+is-limit-smoothing-enabled :set
 'camera-2d+set-limit-smoothing-enabled)

(defgproperty camera-2d+position-smoothing-enabled 'camera-2d :get
 'camera-2d+is-position-smoothing-enabled :set
 'camera-2d+set-position-smoothing-enabled)

(defgproperty camera-2d+position-smoothing-speed 'camera-2d :get
 'camera-2d+get-position-smoothing-speed :set
 'camera-2d+set-position-smoothing-speed)

(defgproperty camera-2d+rotation-smoothing-enabled 'camera-2d :get
 'camera-2d+is-rotation-smoothing-enabled :set
 'camera-2d+set-rotation-smoothing-enabled)

(defgproperty camera-2d+rotation-smoothing-speed 'camera-2d :get
 'camera-2d+get-rotation-smoothing-speed :set
 'camera-2d+set-rotation-smoothing-speed)

(defgproperty camera-2d+drag-horizontal-enabled 'camera-2d :get
 'camera-2d+is-drag-horizontal-enabled :set
 'camera-2d+set-drag-horizontal-enabled)

(defgproperty camera-2d+drag-vertical-enabled 'camera-2d :get
 'camera-2d+is-drag-vertical-enabled :set 'camera-2d+set-drag-vertical-enabled)

(defgproperty camera-2d+drag-horizontal-offset 'camera-2d :get
 'camera-2d+get-drag-horizontal-offset :set
 'camera-2d+set-drag-horizontal-offset)

(defgproperty camera-2d+drag-vertical-offset 'camera-2d :get
 'camera-2d+get-drag-vertical-offset :set 'camera-2d+set-drag-vertical-offset)

(defgproperty camera-2d+drag-left-margin 'camera-2d :index 0 :get
 'camera-2d+get-drag-margin :set 'camera-2d+set-drag-margin)

(defgproperty camera-2d+drag-top-margin 'camera-2d :index 1 :get
 'camera-2d+get-drag-margin :set 'camera-2d+set-drag-margin)

(defgproperty camera-2d+drag-right-margin 'camera-2d :index 2 :get
 'camera-2d+get-drag-margin :set 'camera-2d+set-drag-margin)

(defgproperty camera-2d+drag-bottom-margin 'camera-2d :index 3 :get
 'camera-2d+get-drag-margin :set 'camera-2d+set-drag-margin)

(defgproperty camera-2d+editor-draw-screen 'camera-2d :get
 'camera-2d+is-screen-drawing-enabled :set
 'camera-2d+set-screen-drawing-enabled)

(defgproperty camera-2d+editor-draw-limits 'camera-2d :get
 'camera-2d+is-limit-drawing-enabled :set 'camera-2d+set-limit-drawing-enabled)

(defgproperty camera-2d+editor-draw-drag-margin 'camera-2d :get
 'camera-2d+is-margin-drawing-enabled :set
 'camera-2d+set-margin-drawing-enabled)