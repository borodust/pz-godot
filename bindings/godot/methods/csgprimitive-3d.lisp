(common-lisp:in-package :%godot)


(defgmethod
 (csgprimitive-3d+set-flip-faces :class 'csgprimitive-3d :bind "set_flip_faces"
  :hash 2586408642)
 :void (flip-faces bool))

(defgmethod
 (csgprimitive-3d+get-flip-faces :class 'csgprimitive-3d :bind "get_flip_faces"
  :hash 2240911060)
 bool)