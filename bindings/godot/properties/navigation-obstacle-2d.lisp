(common-lisp:in-package :%godot)


(defgproperty navigation-obstacle-2d+radius 'navigation-obstacle-2d :get
 'navigation-obstacle-2d+get-radius :set 'navigation-obstacle-2d+set-radius)

(defgproperty navigation-obstacle-2d+vertices 'navigation-obstacle-2d :get
 'navigation-obstacle-2d+get-vertices :set 'navigation-obstacle-2d+set-vertices)

(defgproperty navigation-obstacle-2d+affect-navigation-mesh
 'navigation-obstacle-2d :get
 'navigation-obstacle-2d+get-affect-navigation-mesh :set
 'navigation-obstacle-2d+set-affect-navigation-mesh)

(defgproperty navigation-obstacle-2d+carve-navigation-mesh
 'navigation-obstacle-2d :get 'navigation-obstacle-2d+get-carve-navigation-mesh
 :set 'navigation-obstacle-2d+set-carve-navigation-mesh)

(defgproperty navigation-obstacle-2d+avoidance-enabled 'navigation-obstacle-2d
 :get 'navigation-obstacle-2d+get-avoidance-enabled :set
 'navigation-obstacle-2d+set-avoidance-enabled)

(defgproperty navigation-obstacle-2d+velocity 'navigation-obstacle-2d :get
 'navigation-obstacle-2d+get-velocity :set 'navigation-obstacle-2d+set-velocity)

(defgproperty navigation-obstacle-2d+avoidance-layers 'navigation-obstacle-2d
 :get 'navigation-obstacle-2d+get-avoidance-layers :set
 'navigation-obstacle-2d+set-avoidance-layers)