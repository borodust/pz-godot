(common-lisp:in-package :%godot)


(defgmethod
 (gltftexture-sampler+get-mag-filter :class 'gltftexture-sampler :bind
  "get_mag_filter" :hash 3905245786)
 int)

(defgmethod
 (gltftexture-sampler+set-mag-filter :class 'gltftexture-sampler :bind
  "set_mag_filter" :hash 1286410249)
 :void (filter-mode int))

(defgmethod
 (gltftexture-sampler+get-min-filter :class 'gltftexture-sampler :bind
  "get_min_filter" :hash 3905245786)
 int)

(defgmethod
 (gltftexture-sampler+set-min-filter :class 'gltftexture-sampler :bind
  "set_min_filter" :hash 1286410249)
 :void (filter-mode int))

(defgmethod
 (gltftexture-sampler+get-wrap-s :class 'gltftexture-sampler :bind "get_wrap_s"
  :hash 3905245786)
 int)

(defgmethod
 (gltftexture-sampler+set-wrap-s :class 'gltftexture-sampler :bind "set_wrap_s"
  :hash 1286410249)
 :void (wrap-mode int))

(defgmethod
 (gltftexture-sampler+get-wrap-t :class 'gltftexture-sampler :bind "get_wrap_t"
  :hash 3905245786)
 int)

(defgmethod
 (gltftexture-sampler+set-wrap-t :class 'gltftexture-sampler :bind "set_wrap_t"
  :hash 1286410249)
 :void (wrap-mode int))