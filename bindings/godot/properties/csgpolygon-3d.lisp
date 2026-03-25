(common-lisp:in-package :%godot)


(defgproperty csgpolygon-3d+polygon 'csgpolygon-3d :get
 'csgpolygon-3d+get-polygon :set 'csgpolygon-3d+set-polygon)

(defgproperty csgpolygon-3d+mode 'csgpolygon-3d :get 'csgpolygon-3d+get-mode
 :set 'csgpolygon-3d+set-mode)

(defgproperty csgpolygon-3d+depth 'csgpolygon-3d :get 'csgpolygon-3d+get-depth
 :set 'csgpolygon-3d+set-depth)

(defgproperty csgpolygon-3d+spin-degrees 'csgpolygon-3d :get
 'csgpolygon-3d+get-spin-degrees :set 'csgpolygon-3d+set-spin-degrees)

(defgproperty csgpolygon-3d+spin-sides 'csgpolygon-3d :get
 'csgpolygon-3d+get-spin-sides :set 'csgpolygon-3d+set-spin-sides)

(defgproperty csgpolygon-3d+path-node 'csgpolygon-3d :get
 'csgpolygon-3d+get-path-node :set 'csgpolygon-3d+set-path-node)

(defgproperty csgpolygon-3d+path-interval-type 'csgpolygon-3d :get
 'csgpolygon-3d+get-path-interval-type :set
 'csgpolygon-3d+set-path-interval-type)

(defgproperty csgpolygon-3d+path-interval 'csgpolygon-3d :get
 'csgpolygon-3d+get-path-interval :set 'csgpolygon-3d+set-path-interval)

(defgproperty csgpolygon-3d+path-simplify-angle 'csgpolygon-3d :get
 'csgpolygon-3d+get-path-simplify-angle :set
 'csgpolygon-3d+set-path-simplify-angle)

(defgproperty csgpolygon-3d+path-rotation 'csgpolygon-3d :get
 'csgpolygon-3d+get-path-rotation :set 'csgpolygon-3d+set-path-rotation)

(defgproperty csgpolygon-3d+path-rotation-accurate 'csgpolygon-3d :get
 'csgpolygon-3d+get-path-rotation-accurate :set
 'csgpolygon-3d+set-path-rotation-accurate)

(defgproperty csgpolygon-3d+path-local 'csgpolygon-3d :get
 'csgpolygon-3d+is-path-local :set 'csgpolygon-3d+set-path-local)

(defgproperty csgpolygon-3d+path-continuous-u 'csgpolygon-3d :get
 'csgpolygon-3d+is-path-continuous-u :set 'csgpolygon-3d+set-path-continuous-u)

(defgproperty csgpolygon-3d+path-u-distance 'csgpolygon-3d :get
 'csgpolygon-3d+get-path-u-distance :set 'csgpolygon-3d+set-path-u-distance)

(defgproperty csgpolygon-3d+path-joined 'csgpolygon-3d :get
 'csgpolygon-3d+is-path-joined :set 'csgpolygon-3d+set-path-joined)

(defgproperty csgpolygon-3d+smooth-faces 'csgpolygon-3d :get
 'csgpolygon-3d+get-smooth-faces :set 'csgpolygon-3d+set-smooth-faces)

(defgproperty csgpolygon-3d+material 'csgpolygon-3d :get
 'csgpolygon-3d+get-material :set 'csgpolygon-3d+set-material)