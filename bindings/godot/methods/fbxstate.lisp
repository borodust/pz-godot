(common-lisp:in-package :%godot)


(defgmethod
 (fbxstate+get-allow-geometry-helper-nodes :class 'fbxstate :bind
  "get_allow_geometry_helper_nodes" :hash 2240911060)
 bool)

(defgmethod
 (fbxstate+set-allow-geometry-helper-nodes :class 'fbxstate :bind
  "set_allow_geometry_helper_nodes" :hash 2586408642)
 :void (allow bool))