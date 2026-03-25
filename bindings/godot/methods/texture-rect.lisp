(common-lisp:in-package :%godot)


(defgmethod
 (texture-rect+set-texture :class 'texture-rect :bind "set_texture" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (texture-rect+get-texture :class 'texture-rect :bind "get_texture" :hash
  3635182373)
 texture-2d)

(defgmethod
 (texture-rect+set-expand-mode :class 'texture-rect :bind "set_expand_mode"
  :hash 1870766882)
 :void (expand-mode texture-rect+expand-mode))

(defgmethod
 (texture-rect+get-expand-mode :class 'texture-rect :bind "get_expand_mode"
  :hash 3863824733)
 texture-rect+expand-mode)

(defgmethod
 (texture-rect+set-flip-h :class 'texture-rect :bind "set_flip_h" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (texture-rect+is-flipped-h :class 'texture-rect :bind "is_flipped_h" :hash
  36873697)
 bool)

(defgmethod
 (texture-rect+set-flip-v :class 'texture-rect :bind "set_flip_v" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (texture-rect+is-flipped-v :class 'texture-rect :bind "is_flipped_v" :hash
  36873697)
 bool)

(defgmethod
 (texture-rect+set-stretch-mode :class 'texture-rect :bind "set_stretch_mode"
  :hash 58788729)
 :void (stretch-mode texture-rect+stretch-mode))

(defgmethod
 (texture-rect+get-stretch-mode :class 'texture-rect :bind "get_stretch_mode"
  :hash 346396079)
 texture-rect+stretch-mode)