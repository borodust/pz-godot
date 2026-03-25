(common-lisp:in-package :%godot)


(defgmethod
 (texture-layered+-get-format :class 'texture-layered :bind "_get_format" :hash
  3847873762 :virtual common-lisp:t)
 image+format)

(defgmethod
 (texture-layered+-get-layered-type :class 'texture-layered :bind
  "_get_layered_type" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (texture-layered+-get-width :class 'texture-layered :bind "_get_width" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (texture-layered+-get-height :class 'texture-layered :bind "_get_height" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (texture-layered+-get-layers :class 'texture-layered :bind "_get_layers" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (texture-layered+-has-mipmaps :class 'texture-layered :bind "_has_mipmaps"
  :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (texture-layered+-get-layer-data :class 'texture-layered :bind
  "_get_layer_data" :hash 3655284255 :virtual common-lisp:t)
 image (layer-index int))

(defgmethod
 (texture-layered+get-format :class 'texture-layered :bind "get_format" :hash
  3847873762)
 image+format)

(defgmethod
 (texture-layered+get-layered-type :class 'texture-layered :bind
  "get_layered_type" :hash 518123893)
 texture-layered+layered-type)

(defgmethod
 (texture-layered+get-width :class 'texture-layered :bind "get_width" :hash
  3905245786)
 int)

(defgmethod
 (texture-layered+get-height :class 'texture-layered :bind "get_height" :hash
  3905245786)
 int)

(defgmethod
 (texture-layered+get-layers :class 'texture-layered :bind "get_layers" :hash
  3905245786)
 int)

(defgmethod
 (texture-layered+has-mipmaps :class 'texture-layered :bind "has_mipmaps" :hash
  36873697)
 bool)

(defgmethod
 (texture-layered+get-layer-data :class 'texture-layered :bind "get_layer_data"
  :hash 3655284255)
 image (layer int))