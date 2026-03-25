(common-lisp:in-package :%godot)


(defgmethod
 (xrface-tracker+get-blend-shape :class 'xrface-tracker :bind "get_blend_shape"
  :hash 330010046)
 float (blend-shape xrface-tracker+blend-shape-entry))

(defgmethod
 (xrface-tracker+set-blend-shape :class 'xrface-tracker :bind "set_blend_shape"
  :hash 2352588791)
 :void (blend-shape xrface-tracker+blend-shape-entry) (weight float))

(defgmethod
 (xrface-tracker+get-blend-shapes :class 'xrface-tracker :bind
  "get_blend_shapes" :hash 675695659)
 packed-float-32array)

(defgmethod
 (xrface-tracker+set-blend-shapes :class 'xrface-tracker :bind
  "set_blend_shapes" :hash 2899603908)
 :void (weights packed-float-32array))