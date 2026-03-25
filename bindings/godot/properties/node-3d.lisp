(common-lisp:in-package :%godot)


(defgproperty node-3d+transform 'node-3d :get 'node-3d+get-transform :set
 'node-3d+set-transform)

(defgproperty node-3d+global-transform 'node-3d :get
 'node-3d+get-global-transform :set 'node-3d+set-global-transform)

(defgproperty node-3d+position 'node-3d :get 'node-3d+get-position :set
 'node-3d+set-position)

(defgproperty node-3d+rotation 'node-3d :get 'node-3d+get-rotation :set
 'node-3d+set-rotation)

(defgproperty node-3d+rotation-degrees 'node-3d :get
 'node-3d+get-rotation-degrees :set 'node-3d+set-rotation-degrees)

(defgproperty node-3d+quaternion 'node-3d :get 'node-3d+get-quaternion :set
 'node-3d+set-quaternion)

(defgproperty node-3d+basis 'node-3d :get 'node-3d+get-basis :set
 'node-3d+set-basis)

(defgproperty node-3d+scale 'node-3d :get 'node-3d+get-scale :set
 'node-3d+set-scale)

(defgproperty node-3d+rotation-edit-mode 'node-3d :get
 'node-3d+get-rotation-edit-mode :set 'node-3d+set-rotation-edit-mode)

(defgproperty node-3d+rotation-order 'node-3d :get 'node-3d+get-rotation-order
 :set 'node-3d+set-rotation-order)

(defgproperty node-3d+top-level 'node-3d :get 'node-3d+is-set-as-top-level :set
 'node-3d+set-as-top-level)

(defgproperty node-3d+global-position 'node-3d :get
 'node-3d+get-global-position :set 'node-3d+set-global-position)

(defgproperty node-3d+global-basis 'node-3d :get 'node-3d+get-global-basis :set
 'node-3d+set-global-basis)

(defgproperty node-3d+global-rotation 'node-3d :get
 'node-3d+get-global-rotation :set 'node-3d+set-global-rotation)

(defgproperty node-3d+global-rotation-degrees 'node-3d :get
 'node-3d+get-global-rotation-degrees :set 'node-3d+set-global-rotation-degrees)

(defgproperty node-3d+visible 'node-3d :get 'node-3d+is-visible :set
 'node-3d+set-visible)

(defgproperty node-3d+visibility-parent 'node-3d :get
 'node-3d+get-visibility-parent :set 'node-3d+set-visibility-parent)