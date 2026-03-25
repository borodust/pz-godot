(common-lisp:in-package :%godot)


(defgproperty csgtorus-3d+inner-radius 'csgtorus-3d :get
 'csgtorus-3d+get-inner-radius :set 'csgtorus-3d+set-inner-radius)

(defgproperty csgtorus-3d+outer-radius 'csgtorus-3d :get
 'csgtorus-3d+get-outer-radius :set 'csgtorus-3d+set-outer-radius)

(defgproperty csgtorus-3d+sides 'csgtorus-3d :get 'csgtorus-3d+get-sides :set
 'csgtorus-3d+set-sides)

(defgproperty csgtorus-3d+ring-sides 'csgtorus-3d :get
 'csgtorus-3d+get-ring-sides :set 'csgtorus-3d+set-ring-sides)

(defgproperty csgtorus-3d+smooth-faces 'csgtorus-3d :get
 'csgtorus-3d+get-smooth-faces :set 'csgtorus-3d+set-smooth-faces)

(defgproperty csgtorus-3d+material 'csgtorus-3d :get 'csgtorus-3d+get-material
 :set 'csgtorus-3d+set-material)