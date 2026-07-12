(common-lisp:in-package :%godot)


(defgproperty collision-shape-2d+shape 'collision-shape-2d :get
 'collision-shape-2d+get-shape :set 'collision-shape-2d+set-shape)

(defgproperty collision-shape-2d+disabled 'collision-shape-2d :get
 'collision-shape-2d+is-disabled :set 'collision-shape-2d+set-disabled)

(defgproperty collision-shape-2d+one-way-collision 'collision-shape-2d :get
 'collision-shape-2d+is-one-way-collision-enabled :set
 'collision-shape-2d+set-one-way-collision)

(defgproperty collision-shape-2d+one-way-collision-margin 'collision-shape-2d
 :get 'collision-shape-2d+get-one-way-collision-margin :set
 'collision-shape-2d+set-one-way-collision-margin)

(defgproperty collision-shape-2d+one-way-collision-direction
 'collision-shape-2d :get 'collision-shape-2d+get-one-way-collision-direction
 :set 'collision-shape-2d+set-one-way-collision-direction)

(defgproperty collision-shape-2d+debug-color 'collision-shape-2d :get
 'collision-shape-2d+get-debug-color :set 'collision-shape-2d+set-debug-color)