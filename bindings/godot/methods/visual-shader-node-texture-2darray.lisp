(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-texture-2darray+set-texture-array :class
  'visual-shader-node-texture-2darray :bind "set_texture_array" :hash
  1278366092)
 :void (value texture-layered))

(defgmethod
 (visual-shader-node-texture-2darray+get-texture-array :class
  'visual-shader-node-texture-2darray :bind "get_texture_array" :hash
  3984243839)
 texture-layered)