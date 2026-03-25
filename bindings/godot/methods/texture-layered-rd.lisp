(common-lisp:in-package :%godot)


(defgmethod
 (texture-layered-rd+set-texture-rd-rid :class 'texture-layered-rd :bind
  "set_texture_rd_rid" :hash 2722037293)
 :void (texture-rd-rid rid))

(defgmethod
 (texture-layered-rd+get-texture-rd-rid :class 'texture-layered-rd :bind
  "get_texture_rd_rid" :hash 2944877500)
 rid)