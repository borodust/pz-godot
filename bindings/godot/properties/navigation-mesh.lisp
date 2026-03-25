(common-lisp:in-package :%godot)


(defgproperty navigation-mesh+vertices 'navigation-mesh :get
 'navigation-mesh+get-vertices :set 'navigation-mesh+set-vertices)

(defgproperty navigation-mesh+polygons 'navigation-mesh)

(defgproperty navigation-mesh+sample-partition-type 'navigation-mesh :get
 'navigation-mesh+get-sample-partition-type :set
 'navigation-mesh+set-sample-partition-type)

(defgproperty navigation-mesh+geometry-parsed-geometry-type 'navigation-mesh
 :get 'navigation-mesh+get-parsed-geometry-type :set
 'navigation-mesh+set-parsed-geometry-type)

(defgproperty navigation-mesh+geometry-collision-mask 'navigation-mesh :get
 'navigation-mesh+get-collision-mask :set 'navigation-mesh+set-collision-mask)

(defgproperty navigation-mesh+geometry-source-geometry-mode 'navigation-mesh
 :get 'navigation-mesh+get-source-geometry-mode :set
 'navigation-mesh+set-source-geometry-mode)

(defgproperty navigation-mesh+geometry-source-group-name 'navigation-mesh :get
 'navigation-mesh+get-source-group-name :set
 'navigation-mesh+set-source-group-name)

(defgproperty navigation-mesh+cell-size 'navigation-mesh :get
 'navigation-mesh+get-cell-size :set 'navigation-mesh+set-cell-size)

(defgproperty navigation-mesh+cell-height 'navigation-mesh :get
 'navigation-mesh+get-cell-height :set 'navigation-mesh+set-cell-height)

(defgproperty navigation-mesh+border-size 'navigation-mesh :get
 'navigation-mesh+get-border-size :set 'navigation-mesh+set-border-size)

(defgproperty navigation-mesh+agent-height 'navigation-mesh :get
 'navigation-mesh+get-agent-height :set 'navigation-mesh+set-agent-height)

(defgproperty navigation-mesh+agent-radius 'navigation-mesh :get
 'navigation-mesh+get-agent-radius :set 'navigation-mesh+set-agent-radius)

(defgproperty navigation-mesh+agent-max-climb 'navigation-mesh :get
 'navigation-mesh+get-agent-max-climb :set 'navigation-mesh+set-agent-max-climb)

(defgproperty navigation-mesh+agent-max-slope 'navigation-mesh :get
 'navigation-mesh+get-agent-max-slope :set 'navigation-mesh+set-agent-max-slope)

(defgproperty navigation-mesh+region-min-size 'navigation-mesh :get
 'navigation-mesh+get-region-min-size :set 'navigation-mesh+set-region-min-size)

(defgproperty navigation-mesh+region-merge-size 'navigation-mesh :get
 'navigation-mesh+get-region-merge-size :set
 'navigation-mesh+set-region-merge-size)

(defgproperty navigation-mesh+edge-max-length 'navigation-mesh :get
 'navigation-mesh+get-edge-max-length :set 'navigation-mesh+set-edge-max-length)

(defgproperty navigation-mesh+edge-max-error 'navigation-mesh :get
 'navigation-mesh+get-edge-max-error :set 'navigation-mesh+set-edge-max-error)

(defgproperty navigation-mesh+vertices-per-polygon 'navigation-mesh :get
 'navigation-mesh+get-vertices-per-polygon :set
 'navigation-mesh+set-vertices-per-polygon)

(defgproperty navigation-mesh+detail-sample-distance 'navigation-mesh :get
 'navigation-mesh+get-detail-sample-distance :set
 'navigation-mesh+set-detail-sample-distance)

(defgproperty navigation-mesh+detail-sample-max-error 'navigation-mesh :get
 'navigation-mesh+get-detail-sample-max-error :set
 'navigation-mesh+set-detail-sample-max-error)

(defgproperty navigation-mesh+filter-low-hanging-obstacles 'navigation-mesh
 :get 'navigation-mesh+get-filter-low-hanging-obstacles :set
 'navigation-mesh+set-filter-low-hanging-obstacles)

(defgproperty navigation-mesh+filter-ledge-spans 'navigation-mesh :get
 'navigation-mesh+get-filter-ledge-spans :set
 'navigation-mesh+set-filter-ledge-spans)

(defgproperty navigation-mesh+filter-walkable-low-height-spans 'navigation-mesh
 :get 'navigation-mesh+get-filter-walkable-low-height-spans :set
 'navigation-mesh+set-filter-walkable-low-height-spans)

(defgproperty navigation-mesh+filter-baking-aabb 'navigation-mesh :get
 'navigation-mesh+get-filter-baking-aabb :set
 'navigation-mesh+set-filter-baking-aabb)

(defgproperty navigation-mesh+filter-baking-aabb-offset 'navigation-mesh :get
 'navigation-mesh+get-filter-baking-aabb-offset :set
 'navigation-mesh+set-filter-baking-aabb-offset)