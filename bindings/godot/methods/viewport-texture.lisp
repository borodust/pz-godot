(common-lisp:in-package :%godot)


(defgmethod
 (viewport-texture+set-viewport-path-in-scene :class 'viewport-texture :bind
  "set_viewport_path_in_scene" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (viewport-texture+get-viewport-path-in-scene :class 'viewport-texture :bind
  "get_viewport_path_in_scene" :hash 4075236667)
 node-path)