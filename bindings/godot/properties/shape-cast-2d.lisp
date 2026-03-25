(common-lisp:in-package :%godot)


(defgproperty shape-cast-2d+enabled 'shape-cast-2d :get
 'shape-cast-2d+is-enabled :set 'shape-cast-2d+set-enabled)

(defgproperty shape-cast-2d+shape 'shape-cast-2d :get 'shape-cast-2d+get-shape
 :set 'shape-cast-2d+set-shape)

(defgproperty shape-cast-2d+exclude-parent 'shape-cast-2d :get
 'shape-cast-2d+get-exclude-parent-body :set
 'shape-cast-2d+set-exclude-parent-body)

(defgproperty shape-cast-2d+target-position 'shape-cast-2d :get
 'shape-cast-2d+get-target-position :set 'shape-cast-2d+set-target-position)

(defgproperty shape-cast-2d+margin 'shape-cast-2d :get
 'shape-cast-2d+get-margin :set 'shape-cast-2d+set-margin)

(defgproperty shape-cast-2d+max-results 'shape-cast-2d :get
 'shape-cast-2d+get-max-results :set 'shape-cast-2d+set-max-results)

(defgproperty shape-cast-2d+collision-mask 'shape-cast-2d :get
 'shape-cast-2d+get-collision-mask :set 'shape-cast-2d+set-collision-mask)

(defgproperty shape-cast-2d+collision-result 'shape-cast-2d :get
 'shape-cast-2d+get-collision-result)

(defgproperty shape-cast-2d+collide-with-areas 'shape-cast-2d :get
 'shape-cast-2d+is-collide-with-areas-enabled :set
 'shape-cast-2d+set-collide-with-areas)

(defgproperty shape-cast-2d+collide-with-bodies 'shape-cast-2d :get
 'shape-cast-2d+is-collide-with-bodies-enabled :set
 'shape-cast-2d+set-collide-with-bodies)