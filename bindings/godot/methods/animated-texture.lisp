(common-lisp:in-package :%godot)


(defgmethod
 (animated-texture+set-frames :class 'animated-texture :bind "set_frames" :hash
  1286410249)
 :void (frames int))

(defgmethod
 (animated-texture+get-frames :class 'animated-texture :bind "get_frames" :hash
  3905245786)
 int)

(defgmethod
 (animated-texture+set-current-frame :class 'animated-texture :bind
  "set_current_frame" :hash 1286410249)
 :void (frame int))

(defgmethod
 (animated-texture+get-current-frame :class 'animated-texture :bind
  "get_current_frame" :hash 3905245786)
 int)

(defgmethod
 (animated-texture+set-pause :class 'animated-texture :bind "set_pause" :hash
  2586408642)
 :void (pause bool))

(defgmethod
 (animated-texture+get-pause :class 'animated-texture :bind "get_pause" :hash
  36873697)
 bool)

(defgmethod
 (animated-texture+set-one-shot :class 'animated-texture :bind "set_one_shot"
  :hash 2586408642)
 :void (one-shot bool))

(defgmethod
 (animated-texture+get-one-shot :class 'animated-texture :bind "get_one_shot"
  :hash 36873697)
 bool)

(defgmethod
 (animated-texture+set-speed-scale :class 'animated-texture :bind
  "set_speed_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (animated-texture+get-speed-scale :class 'animated-texture :bind
  "get_speed_scale" :hash 1740695150)
 float)

(defgmethod
 (animated-texture+set-frame-texture :class 'animated-texture :bind
  "set_frame_texture" :hash 666127730)
 :void (frame int) (texture texture-2d))

(defgmethod
 (animated-texture+get-frame-texture :class 'animated-texture :bind
  "get_frame_texture" :hash 3536238170)
 texture-2d (frame int))

(defgmethod
 (animated-texture+set-frame-duration :class 'animated-texture :bind
  "set_frame_duration" :hash 1602489585)
 :void (frame int) (duration float))

(defgmethod
 (animated-texture+get-frame-duration :class 'animated-texture :bind
  "get_frame_duration" :hash 2339986948)
 float (frame int))