(common-lisp:in-package :%godot)


(defgproperty ray-cast-2d+enabled 'ray-cast-2d :get 'ray-cast-2d+is-enabled
 :set 'ray-cast-2d+set-enabled)

(defgproperty ray-cast-2d+exclude-parent 'ray-cast-2d :get
 'ray-cast-2d+get-exclude-parent-body :set 'ray-cast-2d+set-exclude-parent-body)

(defgproperty ray-cast-2d+target-position 'ray-cast-2d :get
 'ray-cast-2d+get-target-position :set 'ray-cast-2d+set-target-position)

(defgproperty ray-cast-2d+collision-mask 'ray-cast-2d :get
 'ray-cast-2d+get-collision-mask :set 'ray-cast-2d+set-collision-mask)

(defgproperty ray-cast-2d+hit-from-inside 'ray-cast-2d :get
 'ray-cast-2d+is-hit-from-inside-enabled :set 'ray-cast-2d+set-hit-from-inside)

(defgproperty ray-cast-2d+collide-with-areas 'ray-cast-2d :get
 'ray-cast-2d+is-collide-with-areas-enabled :set
 'ray-cast-2d+set-collide-with-areas)

(defgproperty ray-cast-2d+collide-with-bodies 'ray-cast-2d :get
 'ray-cast-2d+is-collide-with-bodies-enabled :set
 'ray-cast-2d+set-collide-with-bodies)