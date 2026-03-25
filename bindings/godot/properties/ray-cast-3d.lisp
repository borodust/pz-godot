(common-lisp:in-package :%godot)


(defgproperty ray-cast-3d+enabled 'ray-cast-3d :get 'ray-cast-3d+is-enabled
 :set 'ray-cast-3d+set-enabled)

(defgproperty ray-cast-3d+exclude-parent 'ray-cast-3d :get
 'ray-cast-3d+get-exclude-parent-body :set 'ray-cast-3d+set-exclude-parent-body)

(defgproperty ray-cast-3d+target-position 'ray-cast-3d :get
 'ray-cast-3d+get-target-position :set 'ray-cast-3d+set-target-position)

(defgproperty ray-cast-3d+collision-mask 'ray-cast-3d :get
 'ray-cast-3d+get-collision-mask :set 'ray-cast-3d+set-collision-mask)

(defgproperty ray-cast-3d+hit-from-inside 'ray-cast-3d :get
 'ray-cast-3d+is-hit-from-inside-enabled :set 'ray-cast-3d+set-hit-from-inside)

(defgproperty ray-cast-3d+hit-back-faces 'ray-cast-3d :get
 'ray-cast-3d+is-hit-back-faces-enabled :set 'ray-cast-3d+set-hit-back-faces)

(defgproperty ray-cast-3d+collide-with-areas 'ray-cast-3d :get
 'ray-cast-3d+is-collide-with-areas-enabled :set
 'ray-cast-3d+set-collide-with-areas)

(defgproperty ray-cast-3d+collide-with-bodies 'ray-cast-3d :get
 'ray-cast-3d+is-collide-with-bodies-enabled :set
 'ray-cast-3d+set-collide-with-bodies)

(defgproperty ray-cast-3d+debug-shape-custom-color 'ray-cast-3d :get
 'ray-cast-3d+get-debug-shape-custom-color :set
 'ray-cast-3d+set-debug-shape-custom-color)

(defgproperty ray-cast-3d+debug-shape-thickness 'ray-cast-3d :get
 'ray-cast-3d+get-debug-shape-thickness :set
 'ray-cast-3d+set-debug-shape-thickness)