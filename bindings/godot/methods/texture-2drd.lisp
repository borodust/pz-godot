(common-lisp:in-package :%godot)


(defgmethod
 (texture-2drd+set-texture-rd-rid :class 'texture-2drd :bind
  "set_texture_rd_rid" :hash 2722037293)
 :void (texture-rd-rid rid))

(defgmethod
 (texture-2drd+get-texture-rd-rid :class 'texture-2drd :bind
  "get_texture_rd_rid" :hash 2944877500)
 rid)