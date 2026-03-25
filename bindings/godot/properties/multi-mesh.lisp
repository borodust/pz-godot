(common-lisp:in-package :%godot)


(defgproperty multi-mesh+transform-format 'multi-mesh :get
 'multi-mesh+get-transform-format :set 'multi-mesh+set-transform-format)

(defgproperty multi-mesh+use-colors 'multi-mesh :get
 'multi-mesh+is-using-colors :set 'multi-mesh+set-use-colors)

(defgproperty multi-mesh+use-custom-data 'multi-mesh :get
 'multi-mesh+is-using-custom-data :set 'multi-mesh+set-use-custom-data)

(defgproperty multi-mesh+custom-aabb 'multi-mesh :get
 'multi-mesh+get-custom-aabb :set 'multi-mesh+set-custom-aabb)

(defgproperty multi-mesh+instance-count 'multi-mesh :get
 'multi-mesh+get-instance-count :set 'multi-mesh+set-instance-count)

(defgproperty multi-mesh+visible-instance-count 'multi-mesh :get
 'multi-mesh+get-visible-instance-count :set
 'multi-mesh+set-visible-instance-count)

(defgproperty multi-mesh+mesh 'multi-mesh :get 'multi-mesh+get-mesh :set
 'multi-mesh+set-mesh)

(defgproperty multi-mesh+buffer 'multi-mesh :get 'multi-mesh+get-buffer :set
 'multi-mesh+set-buffer)

(defgproperty multi-mesh+transform-array 'multi-mesh)

(defgproperty multi-mesh+transform-2d-array 'multi-mesh)

(defgproperty multi-mesh+color-array 'multi-mesh)

(defgproperty multi-mesh+custom-data-array 'multi-mesh)

(defgproperty multi-mesh+physics-interpolation-quality 'multi-mesh :get
 'multi-mesh+get-physics-interpolation-quality :set
 'multi-mesh+set-physics-interpolation-quality)