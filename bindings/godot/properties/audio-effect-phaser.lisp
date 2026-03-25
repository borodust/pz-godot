(common-lisp:in-package :%godot)


(defgproperty audio-effect-phaser+range-min-hz 'audio-effect-phaser :get
 'audio-effect-phaser+get-range-min-hz :set
 'audio-effect-phaser+set-range-min-hz)

(defgproperty audio-effect-phaser+range-max-hz 'audio-effect-phaser :get
 'audio-effect-phaser+get-range-max-hz :set
 'audio-effect-phaser+set-range-max-hz)

(defgproperty audio-effect-phaser+rate-hz 'audio-effect-phaser :get
 'audio-effect-phaser+get-rate-hz :set 'audio-effect-phaser+set-rate-hz)

(defgproperty audio-effect-phaser+feedback 'audio-effect-phaser :get
 'audio-effect-phaser+get-feedback :set 'audio-effect-phaser+set-feedback)

(defgproperty audio-effect-phaser+depth 'audio-effect-phaser :get
 'audio-effect-phaser+get-depth :set 'audio-effect-phaser+set-depth)