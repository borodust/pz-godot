(common-lisp:in-package :%godot)


(defgproperty curve-3d+closed 'curve-3d :get 'curve-3d+is-closed :set
 'curve-3d+set-closed)

(defgproperty curve-3d+bake-interval 'curve-3d :get 'curve-3d+get-bake-interval
 :set 'curve-3d+set-bake-interval)

(defgproperty curve-3d+point-count 'curve-3d :get 'curve-3d+get-point-count
 :set 'curve-3d+set-point-count)

(defgproperty curve-3d+up-vector-enabled 'curve-3d :get
 'curve-3d+is-up-vector-enabled :set 'curve-3d+set-up-vector-enabled)