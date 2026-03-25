(common-lisp:in-package :%godot)


(defgmethod
 (shape-2d+set-custom-solver-bias :class 'shape-2d :bind
  "set_custom_solver_bias" :hash 373806689)
 :void (bias float))

(defgmethod
 (shape-2d+get-custom-solver-bias :class 'shape-2d :bind
  "get_custom_solver_bias" :hash 1740695150)
 float)

(defgmethod
 (shape-2d+collide :class 'shape-2d :bind "collide" :hash 3709843132) bool
 (local-xform transform-2d) (with-shape shape-2d) (shape-xform transform-2d))

(defgmethod
 (shape-2d+collide-with-motion :class 'shape-2d :bind "collide_with_motion"
  :hash 2869556801)
 bool (local-xform transform-2d) (local-motion vector-2) (with-shape shape-2d)
 (shape-xform transform-2d) (shape-motion vector-2))

(defgmethod
 (shape-2d+collide-and-get-contacts :class 'shape-2d :bind
  "collide_and_get_contacts" :hash 3056932662)
 packed-vector-2array (local-xform transform-2d) (with-shape shape-2d)
 (shape-xform transform-2d))

(defgmethod
 (shape-2d+collide-with-motion-and-get-contacts :class 'shape-2d :bind
  "collide_with_motion_and_get_contacts" :hash 3620351573)
 packed-vector-2array (local-xform transform-2d) (local-motion vector-2)
 (with-shape shape-2d) (shape-xform transform-2d) (shape-motion vector-2))

(defgmethod (shape-2d+draw :class 'shape-2d :bind "draw" :hash 2948539648)
 :void (canvas-item rid) (color color))

(defgmethod
 (shape-2d+get-rect :class 'shape-2d :bind "get_rect" :hash 1639390495) rect-2)