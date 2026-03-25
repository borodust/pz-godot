(common-lisp:in-package :%godot)


(defgmethod
 (canvas-texture+set-diffuse-texture :class 'canvas-texture :bind
  "set_diffuse_texture" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (canvas-texture+get-diffuse-texture :class 'canvas-texture :bind
  "get_diffuse_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (canvas-texture+set-normal-texture :class 'canvas-texture :bind
  "set_normal_texture" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (canvas-texture+get-normal-texture :class 'canvas-texture :bind
  "get_normal_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (canvas-texture+set-specular-texture :class 'canvas-texture :bind
  "set_specular_texture" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (canvas-texture+get-specular-texture :class 'canvas-texture :bind
  "get_specular_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (canvas-texture+set-specular-color :class 'canvas-texture :bind
  "set_specular_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (canvas-texture+get-specular-color :class 'canvas-texture :bind
  "get_specular_color" :hash 3444240500)
 color)

(defgmethod
 (canvas-texture+set-specular-shininess :class 'canvas-texture :bind
  "set_specular_shininess" :hash 373806689)
 :void (shininess float))

(defgmethod
 (canvas-texture+get-specular-shininess :class 'canvas-texture :bind
  "get_specular_shininess" :hash 1740695150)
 float)

(defgmethod
 (canvas-texture+set-texture-filter :class 'canvas-texture :bind
  "set_texture_filter" :hash 1037999706)
 :void (filter canvas-item+texture-filter))

(defgmethod
 (canvas-texture+get-texture-filter :class 'canvas-texture :bind
  "get_texture_filter" :hash 121960042)
 canvas-item+texture-filter)

(defgmethod
 (canvas-texture+set-texture-repeat :class 'canvas-texture :bind
  "set_texture_repeat" :hash 1716472974)
 :void (repeat canvas-item+texture-repeat))

(defgmethod
 (canvas-texture+get-texture-repeat :class 'canvas-texture :bind
  "get_texture_repeat" :hash 2667158319)
 canvas-item+texture-repeat)