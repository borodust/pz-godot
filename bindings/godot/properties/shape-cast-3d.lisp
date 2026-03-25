(common-lisp:in-package :%godot)


(defgproperty shape-cast-3d+enabled 'shape-cast-3d :get
 'shape-cast-3d+is-enabled :set 'shape-cast-3d+set-enabled)

(defgproperty shape-cast-3d+shape 'shape-cast-3d :get 'shape-cast-3d+get-shape
 :set 'shape-cast-3d+set-shape)

(defgproperty shape-cast-3d+exclude-parent 'shape-cast-3d :get
 'shape-cast-3d+get-exclude-parent-body :set
 'shape-cast-3d+set-exclude-parent-body)

(defgproperty shape-cast-3d+target-position 'shape-cast-3d :get
 'shape-cast-3d+get-target-position :set 'shape-cast-3d+set-target-position)

(defgproperty shape-cast-3d+margin 'shape-cast-3d :get
 'shape-cast-3d+get-margin :set 'shape-cast-3d+set-margin)

(defgproperty shape-cast-3d+max-results 'shape-cast-3d :get
 'shape-cast-3d+get-max-results :set 'shape-cast-3d+set-max-results)

(defgproperty shape-cast-3d+collision-mask 'shape-cast-3d :get
 'shape-cast-3d+get-collision-mask :set 'shape-cast-3d+set-collision-mask)

(defgproperty shape-cast-3d+collision-result 'shape-cast-3d :get
 'shape-cast-3d+get-collision-result)

(defgproperty shape-cast-3d+collide-with-areas 'shape-cast-3d :get
 'shape-cast-3d+is-collide-with-areas-enabled :set
 'shape-cast-3d+set-collide-with-areas)

(defgproperty shape-cast-3d+collide-with-bodies 'shape-cast-3d :get
 'shape-cast-3d+is-collide-with-bodies-enabled :set
 'shape-cast-3d+set-collide-with-bodies)

(defgproperty shape-cast-3d+debug-shape-custom-color 'shape-cast-3d :get
 'shape-cast-3d+get-debug-shape-custom-color :set
 'shape-cast-3d+set-debug-shape-custom-color)