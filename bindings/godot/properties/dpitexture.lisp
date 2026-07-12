(common-lisp:in-package :%godot)


(defgproperty dpitexture+fix-alpha-border 'dpitexture :get
 'dpitexture+get-fix-alpha-border :set 'dpitexture+set-fix-alpha-border)

(defgproperty dpitexture+premult-alpha 'dpitexture :get
 'dpitexture+get-premult-alpha :set 'dpitexture+set-premult-alpha)

(defgproperty dpitexture+base-scale 'dpitexture :get 'dpitexture+get-base-scale
 :set 'dpitexture+set-base-scale)

(defgproperty dpitexture+saturation 'dpitexture :get 'dpitexture+get-saturation
 :set 'dpitexture+set-saturation)

(defgproperty dpitexture+color-map 'dpitexture :get 'dpitexture+get-color-map
 :set 'dpitexture+set-color-map)