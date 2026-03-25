(common-lisp:in-package :%godot)


(defgproperty primitive-mesh+material 'primitive-mesh :get
 'primitive-mesh+get-material :set 'primitive-mesh+set-material)

(defgproperty primitive-mesh+custom-aabb 'primitive-mesh :get
 'primitive-mesh+get-custom-aabb :set 'primitive-mesh+set-custom-aabb)

(defgproperty primitive-mesh+flip-faces 'primitive-mesh :get
 'primitive-mesh+get-flip-faces :set 'primitive-mesh+set-flip-faces)

(defgproperty primitive-mesh+add-uv2 'primitive-mesh :get
 'primitive-mesh+get-add-uv2 :set 'primitive-mesh+set-add-uv2)

(defgproperty primitive-mesh+uv2-padding 'primitive-mesh :get
 'primitive-mesh+get-uv2-padding :set 'primitive-mesh+set-uv2-padding)