(common-lisp:in-package :%godot)


(defgmethod
 (rdtexture-view+set-format-override :class 'rdtexture-view :bind
  "set_format_override" :hash 565531219)
 :void (p-member rendering-device+data-format))

(defgmethod
 (rdtexture-view+get-format-override :class 'rdtexture-view :bind
  "get_format_override" :hash 2235804183)
 rendering-device+data-format)

(defgmethod
 (rdtexture-view+set-swizzle-r :class 'rdtexture-view :bind "set_swizzle_r"
  :hash 3833362581)
 :void (p-member rendering-device+texture-swizzle))

(defgmethod
 (rdtexture-view+get-swizzle-r :class 'rdtexture-view :bind "get_swizzle_r"
  :hash 4150792614)
 rendering-device+texture-swizzle)

(defgmethod
 (rdtexture-view+set-swizzle-g :class 'rdtexture-view :bind "set_swizzle_g"
  :hash 3833362581)
 :void (p-member rendering-device+texture-swizzle))

(defgmethod
 (rdtexture-view+get-swizzle-g :class 'rdtexture-view :bind "get_swizzle_g"
  :hash 4150792614)
 rendering-device+texture-swizzle)

(defgmethod
 (rdtexture-view+set-swizzle-b :class 'rdtexture-view :bind "set_swizzle_b"
  :hash 3833362581)
 :void (p-member rendering-device+texture-swizzle))

(defgmethod
 (rdtexture-view+get-swizzle-b :class 'rdtexture-view :bind "get_swizzle_b"
  :hash 4150792614)
 rendering-device+texture-swizzle)

(defgmethod
 (rdtexture-view+set-swizzle-a :class 'rdtexture-view :bind "set_swizzle_a"
  :hash 3833362581)
 :void (p-member rendering-device+texture-swizzle))

(defgmethod
 (rdtexture-view+get-swizzle-a :class 'rdtexture-view :bind "get_swizzle_a"
  :hash 4150792614)
 rendering-device+texture-swizzle)