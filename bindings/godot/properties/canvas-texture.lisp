(common-lisp:in-package :%godot)


(defgproperty canvas-texture+diffuse-texture 'canvas-texture :get
 'canvas-texture+get-diffuse-texture :set 'canvas-texture+set-diffuse-texture)

(defgproperty canvas-texture+normal-texture 'canvas-texture :get
 'canvas-texture+get-normal-texture :set 'canvas-texture+set-normal-texture)

(defgproperty canvas-texture+specular-texture 'canvas-texture :get
 'canvas-texture+get-specular-texture :set 'canvas-texture+set-specular-texture)

(defgproperty canvas-texture+specular-color 'canvas-texture :get
 'canvas-texture+get-specular-color :set 'canvas-texture+set-specular-color)

(defgproperty canvas-texture+specular-shininess 'canvas-texture :get
 'canvas-texture+get-specular-shininess :set
 'canvas-texture+set-specular-shininess)

(defgproperty canvas-texture+texture-filter 'canvas-texture :get
 'canvas-texture+get-texture-filter :set 'canvas-texture+set-texture-filter)

(defgproperty canvas-texture+texture-repeat 'canvas-texture :get
 'canvas-texture+get-texture-repeat :set 'canvas-texture+set-texture-repeat)