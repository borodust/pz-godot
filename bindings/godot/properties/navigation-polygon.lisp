(common-lisp:in-package :%godot)


(defgproperty navigation-polygon+vertices 'navigation-polygon :get
 'navigation-polygon+get-vertices :set 'navigation-polygon+set-vertices)

(defgproperty navigation-polygon+polygons 'navigation-polygon)

(defgproperty navigation-polygon+outlines 'navigation-polygon)

(defgproperty navigation-polygon+sample-partition-type 'navigation-polygon :get
 'navigation-polygon+get-sample-partition-type :set
 'navigation-polygon+set-sample-partition-type)

(defgproperty navigation-polygon+parsed-geometry-type 'navigation-polygon :get
 'navigation-polygon+get-parsed-geometry-type :set
 'navigation-polygon+set-parsed-geometry-type)

(defgproperty navigation-polygon+parsed-collision-mask 'navigation-polygon :get
 'navigation-polygon+get-parsed-collision-mask :set
 'navigation-polygon+set-parsed-collision-mask)

(defgproperty navigation-polygon+source-geometry-mode 'navigation-polygon :get
 'navigation-polygon+get-source-geometry-mode :set
 'navigation-polygon+set-source-geometry-mode)

(defgproperty navigation-polygon+source-geometry-group-name 'navigation-polygon
 :get 'navigation-polygon+get-source-geometry-group-name :set
 'navigation-polygon+set-source-geometry-group-name)

(defgproperty navigation-polygon+cell-size 'navigation-polygon :get
 'navigation-polygon+get-cell-size :set 'navigation-polygon+set-cell-size)

(defgproperty navigation-polygon+border-size 'navigation-polygon :get
 'navigation-polygon+get-border-size :set 'navigation-polygon+set-border-size)

(defgproperty navigation-polygon+agent-radius 'navigation-polygon :get
 'navigation-polygon+get-agent-radius :set 'navigation-polygon+set-agent-radius)

(defgproperty navigation-polygon+baking-rect 'navigation-polygon :get
 'navigation-polygon+get-baking-rect :set 'navigation-polygon+set-baking-rect)

(defgproperty navigation-polygon+baking-rect-offset 'navigation-polygon :get
 'navigation-polygon+get-baking-rect-offset :set
 'navigation-polygon+set-baking-rect-offset)