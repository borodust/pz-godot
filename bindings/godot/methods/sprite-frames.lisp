(common-lisp:in-package :%godot)


(defgmethod
 (sprite-frames+add-animation :class 'sprite-frames :bind "add_animation" :hash
  3304788590)
 :void (anim string-name))

(defgmethod
 (sprite-frames+has-animation :class 'sprite-frames :bind "has_animation" :hash
  2619796661)
 bool (anim string-name))

(defgmethod
 (sprite-frames+duplicate-animation :class 'sprite-frames :bind
  "duplicate_animation" :hash 3740211285)
 :void (anim-from string-name) (anim-to string-name))

(defgmethod
 (sprite-frames+remove-animation :class 'sprite-frames :bind "remove_animation"
  :hash 3304788590)
 :void (anim string-name))

(defgmethod
 (sprite-frames+rename-animation :class 'sprite-frames :bind "rename_animation"
  :hash 3740211285)
 :void (anim string-name) (newname string-name))

(defgmethod
 (sprite-frames+get-animation-names :class 'sprite-frames :bind
  "get_animation_names" :hash 1139954409)
 packed-string-array)

(defgmethod
 (sprite-frames+set-animation-speed :class 'sprite-frames :bind
  "set_animation_speed" :hash 4135858297)
 :void (anim string-name) (fps float))

(defgmethod
 (sprite-frames+get-animation-speed :class 'sprite-frames :bind
  "get_animation_speed" :hash 2349060816)
 float (anim string-name))

(defgmethod
 (sprite-frames+set-animation-loop :class 'sprite-frames :bind
  "set_animation_loop" :hash 2524380260)
 :void (anim string-name) (loop bool))

(defgmethod
 (sprite-frames+get-animation-loop :class 'sprite-frames :bind
  "get_animation_loop" :hash 2619796661)
 bool (anim string-name))

(defgmethod
 (sprite-frames+set-animation-loop-mode :class 'sprite-frames :bind
  "set_animation_loop_mode" :hash 918068248)
 :void (anim string-name) (loop-mode sprite-frames+loop-mode))

(defgmethod
 (sprite-frames+get-animation-loop-mode :class 'sprite-frames :bind
  "get_animation_loop_mode" :hash 3606360228)
 sprite-frames+loop-mode (anim string-name))

(defgmethod
 (sprite-frames+add-frame :class 'sprite-frames :bind "add_frame" :hash
  1351332740)
 :void (anim string-name) (texture texture-2d) (duration float)
 (at-position int))

(defgmethod
 (sprite-frames+set-frame :class 'sprite-frames :bind "set_frame" :hash
  56804795)
 :void (anim string-name) (idx int) (texture texture-2d) (duration float))

(defgmethod
 (sprite-frames+remove-frame :class 'sprite-frames :bind "remove_frame" :hash
  2415702435)
 :void (anim string-name) (idx int))

(defgmethod
 (sprite-frames+get-frame-count :class 'sprite-frames :bind "get_frame_count"
  :hash 2458036349)
 int (anim string-name))

(defgmethod
 (sprite-frames+get-frame-texture :class 'sprite-frames :bind
  "get_frame_texture" :hash 2900517879)
 texture-2d (anim string-name) (idx int))

(defgmethod
 (sprite-frames+get-frame-duration :class 'sprite-frames :bind
  "get_frame_duration" :hash 1129309260)
 float (anim string-name) (idx int))

(defgmethod
 (sprite-frames+clear :class 'sprite-frames :bind "clear" :hash 3304788590)
 :void (anim string-name))

(defgmethod
 (sprite-frames+clear-all :class 'sprite-frames :bind "clear_all" :hash
  3218959716)
 :void)