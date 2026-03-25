(common-lisp:in-package :%godot)


(defgmethod
 (touch-screen-button+set-texture-normal :class 'touch-screen-button :bind
  "set_texture_normal" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (touch-screen-button+get-texture-normal :class 'touch-screen-button :bind
  "get_texture_normal" :hash 3635182373)
 texture-2d)

(defgmethod
 (touch-screen-button+set-texture-pressed :class 'touch-screen-button :bind
  "set_texture_pressed" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (touch-screen-button+get-texture-pressed :class 'touch-screen-button :bind
  "get_texture_pressed" :hash 3635182373)
 texture-2d)

(defgmethod
 (touch-screen-button+set-bitmask :class 'touch-screen-button :bind
  "set_bitmask" :hash 698588216)
 :void (bitmask bit-map))

(defgmethod
 (touch-screen-button+get-bitmask :class 'touch-screen-button :bind
  "get_bitmask" :hash 2459671998)
 bit-map)

(defgmethod
 (touch-screen-button+set-shape :class 'touch-screen-button :bind "set_shape"
  :hash 771364740)
 :void (shape shape-2d))

(defgmethod
 (touch-screen-button+get-shape :class 'touch-screen-button :bind "get_shape"
  :hash 522005891)
 shape-2d)

(defgmethod
 (touch-screen-button+set-shape-centered :class 'touch-screen-button :bind
  "set_shape_centered" :hash 2586408642)
 :void (bool bool))

(defgmethod
 (touch-screen-button+is-shape-centered :class 'touch-screen-button :bind
  "is_shape_centered" :hash 36873697)
 bool)

(defgmethod
 (touch-screen-button+set-shape-visible :class 'touch-screen-button :bind
  "set_shape_visible" :hash 2586408642)
 :void (bool bool))

(defgmethod
 (touch-screen-button+is-shape-visible :class 'touch-screen-button :bind
  "is_shape_visible" :hash 36873697)
 bool)

(defgmethod
 (touch-screen-button+set-action :class 'touch-screen-button :bind "set_action"
  :hash 83702148)
 :void (action string))

(defgmethod
 (touch-screen-button+get-action :class 'touch-screen-button :bind "get_action"
  :hash 201670096)
 string)

(defgmethod
 (touch-screen-button+set-visibility-mode :class 'touch-screen-button :bind
  "set_visibility_mode" :hash 3031128463)
 :void (mode touch-screen-button+visibility-mode))

(defgmethod
 (touch-screen-button+get-visibility-mode :class 'touch-screen-button :bind
  "get_visibility_mode" :hash 2558996468)
 touch-screen-button+visibility-mode)

(defgmethod
 (touch-screen-button+set-passby-press :class 'touch-screen-button :bind
  "set_passby_press" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (touch-screen-button+is-passby-press-enabled :class 'touch-screen-button :bind
  "is_passby_press_enabled" :hash 36873697)
 bool)

(defgmethod
 (touch-screen-button+is-pressed :class 'touch-screen-button :bind "is_pressed"
  :hash 36873697)
 bool)