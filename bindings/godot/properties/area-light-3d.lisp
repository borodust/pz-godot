(common-lisp:in-package :%godot)


(defgproperty area-light-3d+area-range 'area-light-3d :index 4)

(defgproperty area-light-3d+area-attenuation 'area-light-3d :index 6)

(defgproperty area-light-3d+area-normalize-energy 'area-light-3d :get
 'area-light-3d+is-area-normalizing-energy :set
 'area-light-3d+set-area-normalize-energy)

(defgproperty area-light-3d+area-size 'area-light-3d :get
 'area-light-3d+get-area-size :set 'area-light-3d+set-area-size)

(defgproperty area-light-3d+area-texture 'area-light-3d :get
 'area-light-3d+get-area-texture :set 'area-light-3d+set-area-texture)