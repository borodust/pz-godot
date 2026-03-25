(common-lisp:in-package :%godot)


(defgproperty collision-polygon-2d+build-mode 'collision-polygon-2d :get
 'collision-polygon-2d+get-build-mode :set 'collision-polygon-2d+set-build-mode)

(defgproperty collision-polygon-2d+polygon 'collision-polygon-2d :get
 'collision-polygon-2d+get-polygon :set 'collision-polygon-2d+set-polygon)

(defgproperty collision-polygon-2d+disabled 'collision-polygon-2d :get
 'collision-polygon-2d+is-disabled :set 'collision-polygon-2d+set-disabled)

(defgproperty collision-polygon-2d+one-way-collision 'collision-polygon-2d :get
 'collision-polygon-2d+is-one-way-collision-enabled :set
 'collision-polygon-2d+set-one-way-collision)

(defgproperty collision-polygon-2d+one-way-collision-margin
 'collision-polygon-2d :get 'collision-polygon-2d+get-one-way-collision-margin
 :set 'collision-polygon-2d+set-one-way-collision-margin)