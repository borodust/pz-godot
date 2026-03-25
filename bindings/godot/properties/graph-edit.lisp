(common-lisp:in-package :%godot)


(defgproperty graph-edit+scroll-offset 'graph-edit :get
 'graph-edit+get-scroll-offset :set 'graph-edit+set-scroll-offset)

(defgproperty graph-edit+show-grid 'graph-edit :get 'graph-edit+is-showing-grid
 :set 'graph-edit+set-show-grid)

(defgproperty graph-edit+grid-pattern 'graph-edit :get
 'graph-edit+get-grid-pattern :set 'graph-edit+set-grid-pattern)

(defgproperty graph-edit+snapping-enabled 'graph-edit :get
 'graph-edit+is-snapping-enabled :set 'graph-edit+set-snapping-enabled)

(defgproperty graph-edit+snapping-distance 'graph-edit :get
 'graph-edit+get-snapping-distance :set 'graph-edit+set-snapping-distance)

(defgproperty graph-edit+panning-scheme 'graph-edit :get
 'graph-edit+get-panning-scheme :set 'graph-edit+set-panning-scheme)

(defgproperty graph-edit+right-disconnects 'graph-edit :get
 'graph-edit+is-right-disconnects-enabled :set
 'graph-edit+set-right-disconnects)

(defgproperty graph-edit+type-names 'graph-edit :get 'graph-edit+get-type-names
 :set 'graph-edit+set-type-names)

(defgproperty graph-edit+connection-lines-curvature 'graph-edit :get
 'graph-edit+get-connection-lines-curvature :set
 'graph-edit+set-connection-lines-curvature)

(defgproperty graph-edit+connection-lines-thickness 'graph-edit :get
 'graph-edit+get-connection-lines-thickness :set
 'graph-edit+set-connection-lines-thickness)

(defgproperty graph-edit+connection-lines-antialiased 'graph-edit :get
 'graph-edit+is-connection-lines-antialiased :set
 'graph-edit+set-connection-lines-antialiased)

(defgproperty graph-edit+connections 'graph-edit :get
 'graph-edit+get-connection-list :set 'graph-edit+set-connections)

(defgproperty graph-edit+zoom 'graph-edit :get 'graph-edit+get-zoom :set
 'graph-edit+set-zoom)

(defgproperty graph-edit+zoom-min 'graph-edit :get 'graph-edit+get-zoom-min
 :set 'graph-edit+set-zoom-min)

(defgproperty graph-edit+zoom-max 'graph-edit :get 'graph-edit+get-zoom-max
 :set 'graph-edit+set-zoom-max)

(defgproperty graph-edit+zoom-step 'graph-edit :get 'graph-edit+get-zoom-step
 :set 'graph-edit+set-zoom-step)

(defgproperty graph-edit+minimap-enabled 'graph-edit :get
 'graph-edit+is-minimap-enabled :set 'graph-edit+set-minimap-enabled)

(defgproperty graph-edit+minimap-size 'graph-edit :get
 'graph-edit+get-minimap-size :set 'graph-edit+set-minimap-size)

(defgproperty graph-edit+minimap-opacity 'graph-edit :get
 'graph-edit+get-minimap-opacity :set 'graph-edit+set-minimap-opacity)

(defgproperty graph-edit+show-menu 'graph-edit :get 'graph-edit+is-showing-menu
 :set 'graph-edit+set-show-menu)

(defgproperty graph-edit+show-zoom-label 'graph-edit :get
 'graph-edit+is-showing-zoom-label :set 'graph-edit+set-show-zoom-label)

(defgproperty graph-edit+show-zoom-buttons 'graph-edit :get
 'graph-edit+is-showing-zoom-buttons :set 'graph-edit+set-show-zoom-buttons)

(defgproperty graph-edit+show-grid-buttons 'graph-edit :get
 'graph-edit+is-showing-grid-buttons :set 'graph-edit+set-show-grid-buttons)

(defgproperty graph-edit+show-minimap-button 'graph-edit :get
 'graph-edit+is-showing-minimap-button :set 'graph-edit+set-show-minimap-button)

(defgproperty graph-edit+show-arrange-button 'graph-edit :get
 'graph-edit+is-showing-arrange-button :set 'graph-edit+set-show-arrange-button)