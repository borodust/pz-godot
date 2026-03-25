(common-lisp:in-package :%godot)


(defgproperty collision-shape-3d+shape 'collision-shape-3d :get
 'collision-shape-3d+get-shape :set 'collision-shape-3d+set-shape)

(defgproperty collision-shape-3d+disabled 'collision-shape-3d :get
 'collision-shape-3d+is-disabled :set 'collision-shape-3d+set-disabled)

(defgproperty collision-shape-3d+debug-color 'collision-shape-3d :get
 'collision-shape-3d+get-debug-color :set 'collision-shape-3d+set-debug-color)

(defgproperty collision-shape-3d+debug-fill 'collision-shape-3d :get
 'collision-shape-3d+get-enable-debug-fill :set
 'collision-shape-3d+set-enable-debug-fill)