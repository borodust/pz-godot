(common-lisp:in-package :%godot)


(defgmethod
 (texture-3drd+set-texture-rd-rid :class 'texture-3drd :bind
  "set_texture_rd_rid" :hash 2722037293)
 :void (texture-rd-rid rid))

(defgmethod
 (texture-3drd+get-texture-rd-rid :class 'texture-3drd :bind
  "get_texture_rd_rid" :hash 2944877500)
 rid)