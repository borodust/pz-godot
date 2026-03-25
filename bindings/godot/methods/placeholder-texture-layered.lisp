(common-lisp:in-package :%godot)


(defgmethod
 (placeholder-texture-layered+set-size :class 'placeholder-texture-layered
  :bind "set_size" :hash 1130785943)
 :void (size vector-2i))

(defgmethod
 (placeholder-texture-layered+get-size :class 'placeholder-texture-layered
  :bind "get_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (placeholder-texture-layered+set-layers :class 'placeholder-texture-layered
  :bind "set_layers" :hash 1286410249)
 :void (layers int))