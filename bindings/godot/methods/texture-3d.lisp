(common-lisp:in-package :%godot)


(defgmethod
 (texture-3d+%get-format :class 'texture-3d :bind "_get_format" :hash
  3847873762 :virtual common-lisp:t)
 image+format)

(defgmethod
 (texture-3d+%get-width :class 'texture-3d :bind "_get_width" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (texture-3d+%get-height :class 'texture-3d :bind "_get_height" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (texture-3d+%get-depth :class 'texture-3d :bind "_get_depth" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (texture-3d+%has-mipmaps :class 'texture-3d :bind "_has_mipmaps" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (texture-3d+%get-data :class 'texture-3d :bind "_get_data" :hash 3995934104
  :virtual common-lisp:t)
 array)

(defgmethod
 (texture-3d+get-format :class 'texture-3d :bind "get_format" :hash 3847873762)
 image+format)

(defgmethod
 (texture-3d+get-width :class 'texture-3d :bind "get_width" :hash 3905245786)
 int)

(defgmethod
 (texture-3d+get-height :class 'texture-3d :bind "get_height" :hash 3905245786)
 int)

(defgmethod
 (texture-3d+get-depth :class 'texture-3d :bind "get_depth" :hash 3905245786)
 int)

(defgmethod
 (texture-3d+has-mipmaps :class 'texture-3d :bind "has_mipmaps" :hash 36873697)
 bool)

(defgmethod
 (texture-3d+get-data :class 'texture-3d :bind "get_data" :hash 3995934104)
 array)

(defgmethod
 (texture-3d+create-placeholder :class 'texture-3d :bind "create_placeholder"
  :hash 121922552)
 resource)