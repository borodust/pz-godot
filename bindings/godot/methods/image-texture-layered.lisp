(common-lisp:in-package :%godot)


(defgmethod
 (image-texture-layered+create-from-images :class 'image-texture-layered :bind
  "create_from_images" :hash 2785773503)
 error (images array))

(defgmethod
 (image-texture-layered+update-layer :class 'image-texture-layered :bind
  "update_layer" :hash 3331733361)
 :void (image image) (layer int))