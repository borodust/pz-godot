(common-lisp:in-package :%godot)


(defgproperty csgcylinder-3d+radius 'csgcylinder-3d :get
 'csgcylinder-3d+get-radius :set 'csgcylinder-3d+set-radius)

(defgproperty csgcylinder-3d+height 'csgcylinder-3d :get
 'csgcylinder-3d+get-height :set 'csgcylinder-3d+set-height)

(defgproperty csgcylinder-3d+sides 'csgcylinder-3d :get
 'csgcylinder-3d+get-sides :set 'csgcylinder-3d+set-sides)

(defgproperty csgcylinder-3d+cone 'csgcylinder-3d :get 'csgcylinder-3d+is-cone
 :set 'csgcylinder-3d+set-cone)

(defgproperty csgcylinder-3d+smooth-faces 'csgcylinder-3d :get
 'csgcylinder-3d+get-smooth-faces :set 'csgcylinder-3d+set-smooth-faces)

(defgproperty csgcylinder-3d+material 'csgcylinder-3d :get
 'csgcylinder-3d+get-material :set 'csgcylinder-3d+set-material)