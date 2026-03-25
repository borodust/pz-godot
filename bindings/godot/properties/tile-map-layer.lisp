(common-lisp:in-package :%godot)


(defgproperty tile-map-layer+tile-map-data 'tile-map-layer :get
 'tile-map-layer+get-tile-map-data-as-array :set
 'tile-map-layer+set-tile-map-data-from-array)

(defgproperty tile-map-layer+enabled 'tile-map-layer :get
 'tile-map-layer+is-enabled :set 'tile-map-layer+set-enabled)

(defgproperty tile-map-layer+tile-set 'tile-map-layer :get
 'tile-map-layer+get-tile-set :set 'tile-map-layer+set-tile-set)

(defgproperty tile-map-layer+occlusion-enabled 'tile-map-layer :get
 'tile-map-layer+is-occlusion-enabled :set
 'tile-map-layer+set-occlusion-enabled)

(defgproperty tile-map-layer+y-sort-origin 'tile-map-layer :get
 'tile-map-layer+get-y-sort-origin :set 'tile-map-layer+set-y-sort-origin)

(defgproperty tile-map-layer+x-draw-order-reversed 'tile-map-layer :get
 'tile-map-layer+is-x-draw-order-reversed :set
 'tile-map-layer+set-x-draw-order-reversed)

(defgproperty tile-map-layer+rendering-quadrant-size 'tile-map-layer :get
 'tile-map-layer+get-rendering-quadrant-size :set
 'tile-map-layer+set-rendering-quadrant-size)

(defgproperty tile-map-layer+collision-enabled 'tile-map-layer :get
 'tile-map-layer+is-collision-enabled :set
 'tile-map-layer+set-collision-enabled)

(defgproperty tile-map-layer+use-kinematic-bodies 'tile-map-layer :get
 'tile-map-layer+is-using-kinematic-bodies :set
 'tile-map-layer+set-use-kinematic-bodies)

(defgproperty tile-map-layer+collision-visibility-mode 'tile-map-layer :get
 'tile-map-layer+get-collision-visibility-mode :set
 'tile-map-layer+set-collision-visibility-mode)

(defgproperty tile-map-layer+physics-quadrant-size 'tile-map-layer :get
 'tile-map-layer+get-physics-quadrant-size :set
 'tile-map-layer+set-physics-quadrant-size)

(defgproperty tile-map-layer+navigation-enabled 'tile-map-layer :get
 'tile-map-layer+is-navigation-enabled :set
 'tile-map-layer+set-navigation-enabled)

(defgproperty tile-map-layer+navigation-visibility-mode 'tile-map-layer :get
 'tile-map-layer+get-navigation-visibility-mode :set
 'tile-map-layer+set-navigation-visibility-mode)