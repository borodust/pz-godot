(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-phaser+set-range-min-hz :class 'audio-effect-phaser :bind
  "set_range_min_hz" :hash 373806689)
 :void (hz float))

(defgmethod
 (audio-effect-phaser+get-range-min-hz :class 'audio-effect-phaser :bind
  "get_range_min_hz" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-phaser+set-range-max-hz :class 'audio-effect-phaser :bind
  "set_range_max_hz" :hash 373806689)
 :void (hz float))

(defgmethod
 (audio-effect-phaser+get-range-max-hz :class 'audio-effect-phaser :bind
  "get_range_max_hz" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-phaser+set-rate-hz :class 'audio-effect-phaser :bind
  "set_rate_hz" :hash 373806689)
 :void (hz float))

(defgmethod
 (audio-effect-phaser+get-rate-hz :class 'audio-effect-phaser :bind
  "get_rate_hz" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-phaser+set-feedback :class 'audio-effect-phaser :bind
  "set_feedback" :hash 373806689)
 :void (fbk float))

(defgmethod
 (audio-effect-phaser+get-feedback :class 'audio-effect-phaser :bind
  "get_feedback" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-phaser+set-depth :class 'audio-effect-phaser :bind "set_depth"
  :hash 373806689)
 :void (depth float))

(defgmethod
 (audio-effect-phaser+get-depth :class 'audio-effect-phaser :bind "get_depth"
  :hash 1740695150)
 float)