(common-lisp:in-package :%godot)


(defgmethod
 (framebuffer-cache-rd+get-cache-multipass :class 'framebuffer-cache-rd :bind
  "get_cache_multipass" :hash 3437881813 :static common-lisp:t)
 rid (textures array) (passes array) (views int))