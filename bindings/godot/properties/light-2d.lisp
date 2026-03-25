(common-lisp:in-package :%godot)


(defgproperty light-2d+enabled 'light-2d :get 'light-2d+is-enabled :set
 'light-2d+set-enabled)

(defgproperty light-2d+editor-only 'light-2d :get 'light-2d+is-editor-only :set
 'light-2d+set-editor-only)

(defgproperty light-2d+color 'light-2d :get 'light-2d+get-color :set
 'light-2d+set-color)

(defgproperty light-2d+energy 'light-2d :get 'light-2d+get-energy :set
 'light-2d+set-energy)

(defgproperty light-2d+blend-mode 'light-2d :get 'light-2d+get-blend-mode :set
 'light-2d+set-blend-mode)

(defgproperty light-2d+range-z-min 'light-2d :get 'light-2d+get-z-range-min
 :set 'light-2d+set-z-range-min)

(defgproperty light-2d+range-z-max 'light-2d :get 'light-2d+get-z-range-max
 :set 'light-2d+set-z-range-max)

(defgproperty light-2d+range-layer-min 'light-2d :get
 'light-2d+get-layer-range-min :set 'light-2d+set-layer-range-min)

(defgproperty light-2d+range-layer-max 'light-2d :get
 'light-2d+get-layer-range-max :set 'light-2d+set-layer-range-max)

(defgproperty light-2d+range-item-cull-mask 'light-2d :get
 'light-2d+get-item-cull-mask :set 'light-2d+set-item-cull-mask)

(defgproperty light-2d+shadow-enabled 'light-2d :get
 'light-2d+is-shadow-enabled :set 'light-2d+set-shadow-enabled)

(defgproperty light-2d+shadow-color 'light-2d :get 'light-2d+get-shadow-color
 :set 'light-2d+set-shadow-color)

(defgproperty light-2d+shadow-filter 'light-2d :get 'light-2d+get-shadow-filter
 :set 'light-2d+set-shadow-filter)

(defgproperty light-2d+shadow-filter-smooth 'light-2d :get
 'light-2d+get-shadow-smooth :set 'light-2d+set-shadow-smooth)

(defgproperty light-2d+shadow-item-cull-mask 'light-2d :get
 'light-2d+get-item-shadow-cull-mask :set 'light-2d+set-item-shadow-cull-mask)