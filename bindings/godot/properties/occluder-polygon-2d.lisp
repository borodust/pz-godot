(common-lisp:in-package :%godot)


(defgproperty occluder-polygon-2d+closed 'occluder-polygon-2d :get
 'occluder-polygon-2d+is-closed :set 'occluder-polygon-2d+set-closed)

(defgproperty occluder-polygon-2d+cull-mode 'occluder-polygon-2d :get
 'occluder-polygon-2d+get-cull-mode :set 'occluder-polygon-2d+set-cull-mode)

(defgproperty occluder-polygon-2d+polygon 'occluder-polygon-2d :get
 'occluder-polygon-2d+get-polygon :set 'occluder-polygon-2d+set-polygon)