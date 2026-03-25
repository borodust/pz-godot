(common-lisp:in-package :%godot)


(defgproperty navigation-obstacle-3d+radius 'navigation-obstacle-3d :get
 'navigation-obstacle-3d+get-radius :set 'navigation-obstacle-3d+set-radius)

(defgproperty navigation-obstacle-3d+height 'navigation-obstacle-3d :get
 'navigation-obstacle-3d+get-height :set 'navigation-obstacle-3d+set-height)

(defgproperty navigation-obstacle-3d+vertices 'navigation-obstacle-3d :get
 'navigation-obstacle-3d+get-vertices :set 'navigation-obstacle-3d+set-vertices)

(defgproperty navigation-obstacle-3d+affect-navigation-mesh
 'navigation-obstacle-3d :get
 'navigation-obstacle-3d+get-affect-navigation-mesh :set
 'navigation-obstacle-3d+set-affect-navigation-mesh)

(defgproperty navigation-obstacle-3d+carve-navigation-mesh
 'navigation-obstacle-3d :get 'navigation-obstacle-3d+get-carve-navigation-mesh
 :set 'navigation-obstacle-3d+set-carve-navigation-mesh)

(defgproperty navigation-obstacle-3d+avoidance-enabled 'navigation-obstacle-3d
 :get 'navigation-obstacle-3d+get-avoidance-enabled :set
 'navigation-obstacle-3d+set-avoidance-enabled)

(defgproperty navigation-obstacle-3d+velocity 'navigation-obstacle-3d :get
 'navigation-obstacle-3d+get-velocity :set 'navigation-obstacle-3d+set-velocity)

(defgproperty navigation-obstacle-3d+avoidance-layers 'navigation-obstacle-3d
 :get 'navigation-obstacle-3d+get-avoidance-layers :set
 'navigation-obstacle-3d+set-avoidance-layers)

(defgproperty navigation-obstacle-3d+use-3d-avoidance 'navigation-obstacle-3d
 :get 'navigation-obstacle-3d+get-use-3d-avoidance :set
 'navigation-obstacle-3d+set-use-3d-avoidance)