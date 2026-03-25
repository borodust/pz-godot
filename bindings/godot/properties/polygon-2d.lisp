(common-lisp:in-package :%godot)


(defgproperty polygon-2d+color 'polygon-2d :get 'polygon-2d+get-color :set
 'polygon-2d+set-color)

(defgproperty polygon-2d+offset 'polygon-2d :get 'polygon-2d+get-offset :set
 'polygon-2d+set-offset)

(defgproperty polygon-2d+antialiased 'polygon-2d :get
 'polygon-2d+get-antialiased :set 'polygon-2d+set-antialiased)

(defgproperty polygon-2d+texture 'polygon-2d :get 'polygon-2d+get-texture :set
 'polygon-2d+set-texture)

(defgproperty polygon-2d+texture-offset 'polygon-2d :get
 'polygon-2d+get-texture-offset :set 'polygon-2d+set-texture-offset)

(defgproperty polygon-2d+texture-scale 'polygon-2d :get
 'polygon-2d+get-texture-scale :set 'polygon-2d+set-texture-scale)

(defgproperty polygon-2d+texture-rotation 'polygon-2d :get
 'polygon-2d+get-texture-rotation :set 'polygon-2d+set-texture-rotation)

(defgproperty polygon-2d+skeleton 'polygon-2d :get 'polygon-2d+get-skeleton
 :set 'polygon-2d+set-skeleton)

(defgproperty polygon-2d+invert-enabled 'polygon-2d :get
 'polygon-2d+get-invert-enabled :set 'polygon-2d+set-invert-enabled)

(defgproperty polygon-2d+invert-border 'polygon-2d :get
 'polygon-2d+get-invert-border :set 'polygon-2d+set-invert-border)

(defgproperty polygon-2d+polygon 'polygon-2d :get 'polygon-2d+get-polygon :set
 'polygon-2d+set-polygon)

(defgproperty polygon-2d+uv 'polygon-2d :get 'polygon-2d+get-uv :set
 'polygon-2d+set-uv)

(defgproperty polygon-2d+vertex-colors 'polygon-2d :get
 'polygon-2d+get-vertex-colors :set 'polygon-2d+set-vertex-colors)

(defgproperty polygon-2d+polygons 'polygon-2d :get 'polygon-2d+get-polygons
 :set 'polygon-2d+set-polygons)

(defgproperty polygon-2d+bones 'polygon-2d)

(defgproperty polygon-2d+internal-vertex-count 'polygon-2d :get
 'polygon-2d+get-internal-vertex-count :set
 'polygon-2d+set-internal-vertex-count)