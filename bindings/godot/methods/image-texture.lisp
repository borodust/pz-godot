(common-lisp:in-package :%godot)


(defgmethod
 (image-texture+create-from-image :class 'image-texture :bind
  "create_from_image" :hash 2775144163 :static common-lisp:t)
 image-texture (image image))

(defgmethod
 (image-texture+get-format :class 'image-texture :bind "get_format" :hash
  3847873762)
 image+format)

(defgmethod
 (image-texture+set-image :class 'image-texture :bind "set_image" :hash
  532598488)
 :void (image image))

(defgmethod
 (image-texture+update :class 'image-texture :bind "update" :hash 532598488)
 :void (image image))

(defgmethod
 (image-texture+set-size-override :class 'image-texture :bind
  "set_size_override" :hash 1130785943)
 :void (size vector-2i))