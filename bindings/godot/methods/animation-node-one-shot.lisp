(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-one-shot+set-fadein-time :class 'animation-node-one-shot :bind
  "set_fadein_time" :hash 373806689)
 :void (time float))

(defgmethod
 (animation-node-one-shot+get-fadein-time :class 'animation-node-one-shot :bind
  "get_fadein_time" :hash 1740695150)
 float)

(defgmethod
 (animation-node-one-shot+set-fadein-curve :class 'animation-node-one-shot
  :bind "set_fadein_curve" :hash 270443179)
 :void (curve curve))

(defgmethod
 (animation-node-one-shot+get-fadein-curve :class 'animation-node-one-shot
  :bind "get_fadein_curve" :hash 2460114913)
 curve)

(defgmethod
 (animation-node-one-shot+set-fadeout-time :class 'animation-node-one-shot
  :bind "set_fadeout_time" :hash 373806689)
 :void (time float))

(defgmethod
 (animation-node-one-shot+get-fadeout-time :class 'animation-node-one-shot
  :bind "get_fadeout_time" :hash 1740695150)
 float)

(defgmethod
 (animation-node-one-shot+set-fadeout-curve :class 'animation-node-one-shot
  :bind "set_fadeout_curve" :hash 270443179)
 :void (curve curve))

(defgmethod
 (animation-node-one-shot+get-fadeout-curve :class 'animation-node-one-shot
  :bind "get_fadeout_curve" :hash 2460114913)
 curve)

(defgmethod
 (animation-node-one-shot+set-break-loop-at-end :class 'animation-node-one-shot
  :bind "set_break_loop_at_end" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-one-shot+is-loop-broken-at-end :class 'animation-node-one-shot
  :bind "is_loop_broken_at_end" :hash 36873697)
 bool)

(defgmethod
 (animation-node-one-shot+set-abort-on-reset :class 'animation-node-one-shot
  :bind "set_abort_on_reset" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-one-shot+is-aborted-on-reset :class 'animation-node-one-shot
  :bind "is_aborted_on_reset" :hash 36873697)
 bool)

(defgmethod
 (animation-node-one-shot+set-autorestart :class 'animation-node-one-shot :bind
  "set_autorestart" :hash 2586408642)
 :void (active bool))

(defgmethod
 (animation-node-one-shot+has-autorestart :class 'animation-node-one-shot :bind
  "has_autorestart" :hash 36873697)
 bool)

(defgmethod
 (animation-node-one-shot+set-autorestart-delay :class 'animation-node-one-shot
  :bind "set_autorestart_delay" :hash 373806689)
 :void (time float))

(defgmethod
 (animation-node-one-shot+get-autorestart-delay :class 'animation-node-one-shot
  :bind "get_autorestart_delay" :hash 1740695150)
 float)

(defgmethod
 (animation-node-one-shot+set-autorestart-random-delay :class
  'animation-node-one-shot :bind "set_autorestart_random_delay" :hash
  373806689)
 :void (time float))

(defgmethod
 (animation-node-one-shot+get-autorestart-random-delay :class
  'animation-node-one-shot :bind "get_autorestart_random_delay" :hash
  1740695150)
 float)

(defgmethod
 (animation-node-one-shot+set-mix-mode :class 'animation-node-one-shot :bind
  "set_mix_mode" :hash 1018899799)
 :void (mode animation-node-one-shot+mix-mode))

(defgmethod
 (animation-node-one-shot+get-mix-mode :class 'animation-node-one-shot :bind
  "get_mix_mode" :hash 3076550526)
 animation-node-one-shot+mix-mode)