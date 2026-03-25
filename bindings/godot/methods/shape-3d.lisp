(common-lisp:in-package :%godot)


(defgmethod
 (shape-3d+set-custom-solver-bias :class 'shape-3d :bind
  "set_custom_solver_bias" :hash 373806689)
 :void (bias float))

(defgmethod
 (shape-3d+get-custom-solver-bias :class 'shape-3d :bind
  "get_custom_solver_bias" :hash 1740695150)
 float)

(defgmethod
 (shape-3d+set-margin :class 'shape-3d :bind "set_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (shape-3d+get-margin :class 'shape-3d :bind "get_margin" :hash 1740695150)
 float)

(defgmethod
 (shape-3d+get-debug-mesh :class 'shape-3d :bind "get_debug_mesh" :hash
  1605880883)
 array-mesh)