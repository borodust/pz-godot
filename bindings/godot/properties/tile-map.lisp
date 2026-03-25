(common-lisp:in-package :%godot)


(defgproperty tile-map+tile-set 'tile-map :get 'tile-map+get-tileset :set
 'tile-map+set-tileset)

(defgproperty tile-map+rendering-quadrant-size 'tile-map :get
 'tile-map+get-rendering-quadrant-size :set
 'tile-map+set-rendering-quadrant-size)

(defgproperty tile-map+collision-animatable 'tile-map :get
 'tile-map+is-collision-animatable :set 'tile-map+set-collision-animatable)

(defgproperty tile-map+collision-visibility-mode 'tile-map :get
 'tile-map+get-collision-visibility-mode :set
 'tile-map+set-collision-visibility-mode)

(defgproperty tile-map+navigation-visibility-mode 'tile-map :get
 'tile-map+get-navigation-visibility-mode :set
 'tile-map+set-navigation-visibility-mode)