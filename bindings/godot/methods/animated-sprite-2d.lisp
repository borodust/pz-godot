(common-lisp:in-package :%godot)


(defgmethod
 (animated-sprite-2d+set-sprite-frames :class 'animated-sprite-2d :bind
  "set_sprite_frames" :hash 905781144)
 :void (sprite-frames sprite-frames))

(defgmethod
 (animated-sprite-2d+get-sprite-frames :class 'animated-sprite-2d :bind
  "get_sprite_frames" :hash 3804851214)
 sprite-frames)

(defgmethod
 (animated-sprite-2d+set-animation :class 'animated-sprite-2d :bind
  "set_animation" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (animated-sprite-2d+get-animation :class 'animated-sprite-2d :bind
  "get_animation" :hash 2002593661)
 string-name)

(defgmethod
 (animated-sprite-2d+set-autoplay :class 'animated-sprite-2d :bind
  "set_autoplay" :hash 83702148)
 :void (name string))

(defgmethod
 (animated-sprite-2d+get-autoplay :class 'animated-sprite-2d :bind
  "get_autoplay" :hash 201670096)
 string)

(defgmethod
 (animated-sprite-2d+is-playing :class 'animated-sprite-2d :bind "is_playing"
  :hash 36873697)
 bool)

(defgmethod
 (animated-sprite-2d+play :class 'animated-sprite-2d :bind "play" :hash
  3269405555)
 :void (name string-name) (custom-speed float) (from-end bool))

(defgmethod
 (animated-sprite-2d+play-backwards :class 'animated-sprite-2d :bind
  "play_backwards" :hash 3323268493)
 :void (name string-name))

(defgmethod
 (animated-sprite-2d+pause :class 'animated-sprite-2d :bind "pause" :hash
  3218959716)
 :void)

(defgmethod
 (animated-sprite-2d+stop :class 'animated-sprite-2d :bind "stop" :hash
  3218959716)
 :void)

(defgmethod
 (animated-sprite-2d+set-centered :class 'animated-sprite-2d :bind
  "set_centered" :hash 2586408642)
 :void (centered bool))

(defgmethod
 (animated-sprite-2d+is-centered :class 'animated-sprite-2d :bind "is_centered"
  :hash 36873697)
 bool)

(defgmethod
 (animated-sprite-2d+set-offset :class 'animated-sprite-2d :bind "set_offset"
  :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (animated-sprite-2d+get-offset :class 'animated-sprite-2d :bind "get_offset"
  :hash 3341600327)
 vector-2)

(defgmethod
 (animated-sprite-2d+set-flip-h :class 'animated-sprite-2d :bind "set_flip_h"
  :hash 2586408642)
 :void (flip-h bool))

(defgmethod
 (animated-sprite-2d+is-flipped-h :class 'animated-sprite-2d :bind
  "is_flipped_h" :hash 36873697)
 bool)

(defgmethod
 (animated-sprite-2d+set-flip-v :class 'animated-sprite-2d :bind "set_flip_v"
  :hash 2586408642)
 :void (flip-v bool))

(defgmethod
 (animated-sprite-2d+is-flipped-v :class 'animated-sprite-2d :bind
  "is_flipped_v" :hash 36873697)
 bool)

(defgmethod
 (animated-sprite-2d+set-frame :class 'animated-sprite-2d :bind "set_frame"
  :hash 1286410249)
 :void (frame int))

(defgmethod
 (animated-sprite-2d+get-frame :class 'animated-sprite-2d :bind "get_frame"
  :hash 3905245786)
 int)

(defgmethod
 (animated-sprite-2d+set-frame-progress :class 'animated-sprite-2d :bind
  "set_frame_progress" :hash 373806689)
 :void (progress float))

(defgmethod
 (animated-sprite-2d+get-frame-progress :class 'animated-sprite-2d :bind
  "get_frame_progress" :hash 1740695150)
 float)

(defgmethod
 (animated-sprite-2d+set-frame-and-progress :class 'animated-sprite-2d :bind
  "set_frame_and_progress" :hash 1602489585)
 :void (frame int) (progress float))

(defgmethod
 (animated-sprite-2d+set-speed-scale :class 'animated-sprite-2d :bind
  "set_speed_scale" :hash 373806689)
 :void (speed-scale float))

(defgmethod
 (animated-sprite-2d+get-speed-scale :class 'animated-sprite-2d :bind
  "get_speed_scale" :hash 1740695150)
 float)

(defgmethod
 (animated-sprite-2d+get-playing-speed :class 'animated-sprite-2d :bind
  "get_playing_speed" :hash 1740695150)
 float)