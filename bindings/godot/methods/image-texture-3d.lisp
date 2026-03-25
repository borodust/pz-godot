(common-lisp:in-package :%godot)


(defgmethod
 (image-texture-3d+create :class 'image-texture-3d :bind "create" :hash
  1130379827)
 error (format image+format) (width int) (height int) (depth int)
 (use-mipmaps bool) (data array))

(defgmethod
 (image-texture-3d+update :class 'image-texture-3d :bind "update" :hash
  381264803)
 :void (data array))