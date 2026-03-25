(common-lisp:in-package :%godot)


(defgmethod
 (texture-button+set-texture-normal :class 'texture-button :bind
  "set_texture_normal" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (texture-button+set-texture-pressed :class 'texture-button :bind
  "set_texture_pressed" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (texture-button+set-texture-hover :class 'texture-button :bind
  "set_texture_hover" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (texture-button+set-texture-disabled :class 'texture-button :bind
  "set_texture_disabled" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (texture-button+set-texture-focused :class 'texture-button :bind
  "set_texture_focused" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (texture-button+set-click-mask :class 'texture-button :bind "set_click_mask"
  :hash 698588216)
 :void (mask bit-map))

(defgmethod
 (texture-button+set-ignore-texture-size :class 'texture-button :bind
  "set_ignore_texture_size" :hash 2586408642)
 :void (ignore bool))

(defgmethod
 (texture-button+set-stretch-mode :class 'texture-button :bind
  "set_stretch_mode" :hash 252530840)
 :void (mode texture-button+stretch-mode))

(defgmethod
 (texture-button+set-flip-h :class 'texture-button :bind "set_flip_h" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (texture-button+is-flipped-h :class 'texture-button :bind "is_flipped_h" :hash
  36873697)
 bool)

(defgmethod
 (texture-button+set-flip-v :class 'texture-button :bind "set_flip_v" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (texture-button+is-flipped-v :class 'texture-button :bind "is_flipped_v" :hash
  36873697)
 bool)

(defgmethod
 (texture-button+get-texture-normal :class 'texture-button :bind
  "get_texture_normal" :hash 3635182373)
 texture-2d)

(defgmethod
 (texture-button+get-texture-pressed :class 'texture-button :bind
  "get_texture_pressed" :hash 3635182373)
 texture-2d)

(defgmethod
 (texture-button+get-texture-hover :class 'texture-button :bind
  "get_texture_hover" :hash 3635182373)
 texture-2d)

(defgmethod
 (texture-button+get-texture-disabled :class 'texture-button :bind
  "get_texture_disabled" :hash 3635182373)
 texture-2d)

(defgmethod
 (texture-button+get-texture-focused :class 'texture-button :bind
  "get_texture_focused" :hash 3635182373)
 texture-2d)

(defgmethod
 (texture-button+get-click-mask :class 'texture-button :bind "get_click_mask"
  :hash 2459671998)
 bit-map)

(defgmethod
 (texture-button+get-ignore-texture-size :class 'texture-button :bind
  "get_ignore_texture_size" :hash 36873697)
 bool)

(defgmethod
 (texture-button+get-stretch-mode :class 'texture-button :bind
  "get_stretch_mode" :hash 33815122)
 texture-button+stretch-mode)