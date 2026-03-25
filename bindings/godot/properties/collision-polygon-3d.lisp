(common-lisp:in-package :%godot)


(defgproperty collision-polygon-3d+depth 'collision-polygon-3d :get
 'collision-polygon-3d+get-depth :set 'collision-polygon-3d+set-depth)

(defgproperty collision-polygon-3d+disabled 'collision-polygon-3d :get
 'collision-polygon-3d+is-disabled :set 'collision-polygon-3d+set-disabled)

(defgproperty collision-polygon-3d+polygon 'collision-polygon-3d :get
 'collision-polygon-3d+get-polygon :set 'collision-polygon-3d+set-polygon)

(defgproperty collision-polygon-3d+margin 'collision-polygon-3d :get
 'collision-polygon-3d+get-margin :set 'collision-polygon-3d+set-margin)

(defgproperty collision-polygon-3d+debug-color 'collision-polygon-3d :get
 'collision-polygon-3d+get-debug-color :set
 'collision-polygon-3d+set-debug-color)

(defgproperty collision-polygon-3d+debug-fill 'collision-polygon-3d :get
 'collision-polygon-3d+get-enable-debug-fill :set
 'collision-polygon-3d+set-enable-debug-fill)