(common-lisp:in-package :%godot)


(defgmethod
 (world-boundary-shape-3d+set-plane :class 'world-boundary-shape-3d :bind
  "set_plane" :hash 3505987427)
 :void (plane plane))

(defgmethod
 (world-boundary-shape-3d+get-plane :class 'world-boundary-shape-3d :bind
  "get_plane" :hash 2753500971)
 plane)