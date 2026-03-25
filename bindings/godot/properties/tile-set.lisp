(common-lisp:in-package :%godot)


(defgproperty tile-set+tile-shape 'tile-set :get 'tile-set+get-tile-shape :set
 'tile-set+set-tile-shape)

(defgproperty tile-set+tile-layout 'tile-set :get 'tile-set+get-tile-layout
 :set 'tile-set+set-tile-layout)

(defgproperty tile-set+tile-offset-axis 'tile-set :get
 'tile-set+get-tile-offset-axis :set 'tile-set+set-tile-offset-axis)

(defgproperty tile-set+tile-size 'tile-set :get 'tile-set+get-tile-size :set
 'tile-set+set-tile-size)

(defgproperty tile-set+uv-clipping 'tile-set :get 'tile-set+is-uv-clipping :set
 'tile-set+set-uv-clipping)