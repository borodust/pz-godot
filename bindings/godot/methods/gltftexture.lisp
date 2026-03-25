(common-lisp:in-package :%godot)


(defgmethod
 (gltftexture+get-src-image :class 'gltftexture :bind "get_src_image" :hash
  3905245786)
 int)

(defgmethod
 (gltftexture+set-src-image :class 'gltftexture :bind "set_src_image" :hash
  1286410249)
 :void (src-image int))

(defgmethod
 (gltftexture+get-sampler :class 'gltftexture :bind "get_sampler" :hash
  3905245786)
 int)

(defgmethod
 (gltftexture+set-sampler :class 'gltftexture :bind "set_sampler" :hash
  1286410249)
 :void (sampler int))