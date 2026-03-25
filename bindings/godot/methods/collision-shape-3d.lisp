(common-lisp:in-package :%godot)


(defgmethod
 (collision-shape-3d+resource-changed :class 'collision-shape-3d :bind
  "resource_changed" :hash 968641751)
 :void (resource resource))

(defgmethod
 (collision-shape-3d+set-shape :class 'collision-shape-3d :bind "set_shape"
  :hash 1549710052)
 :void (shape shape-3d))

(defgmethod
 (collision-shape-3d+get-shape :class 'collision-shape-3d :bind "get_shape"
  :hash 3214262478)
 shape-3d)

(defgmethod
 (collision-shape-3d+set-disabled :class 'collision-shape-3d :bind
  "set_disabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (collision-shape-3d+is-disabled :class 'collision-shape-3d :bind "is_disabled"
  :hash 36873697)
 bool)

(defgmethod
 (collision-shape-3d+make-convex-from-siblings :class 'collision-shape-3d :bind
  "make_convex_from_siblings" :hash 3218959716)
 :void)

(defgmethod
 (collision-shape-3d+set-debug-color :class 'collision-shape-3d :bind
  "set_debug_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (collision-shape-3d+get-debug-color :class 'collision-shape-3d :bind
  "get_debug_color" :hash 3444240500)
 color)

(defgmethod
 (collision-shape-3d+set-enable-debug-fill :class 'collision-shape-3d :bind
  "set_enable_debug_fill" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (collision-shape-3d+get-enable-debug-fill :class 'collision-shape-3d :bind
  "get_enable_debug_fill" :hash 36873697)
 bool)