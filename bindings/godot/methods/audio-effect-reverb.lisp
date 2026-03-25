(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-reverb+set-predelay-msec :class 'audio-effect-reverb :bind
  "set_predelay_msec" :hash 373806689)
 :void (msec float))

(defgmethod
 (audio-effect-reverb+get-predelay-msec :class 'audio-effect-reverb :bind
  "get_predelay_msec" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-reverb+set-predelay-feedback :class 'audio-effect-reverb :bind
  "set_predelay_feedback" :hash 373806689)
 :void (feedback float))

(defgmethod
 (audio-effect-reverb+get-predelay-feedback :class 'audio-effect-reverb :bind
  "get_predelay_feedback" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-reverb+set-room-size :class 'audio-effect-reverb :bind
  "set_room_size" :hash 373806689)
 :void (size float))

(defgmethod
 (audio-effect-reverb+get-room-size :class 'audio-effect-reverb :bind
  "get_room_size" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-reverb+set-damping :class 'audio-effect-reverb :bind
  "set_damping" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-reverb+get-damping :class 'audio-effect-reverb :bind
  "get_damping" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-reverb+set-spread :class 'audio-effect-reverb :bind "set_spread"
  :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-reverb+get-spread :class 'audio-effect-reverb :bind "get_spread"
  :hash 1740695150)
 float)

(defgmethod
 (audio-effect-reverb+set-dry :class 'audio-effect-reverb :bind "set_dry" :hash
  373806689)
 :void (amount float))

(defgmethod
 (audio-effect-reverb+get-dry :class 'audio-effect-reverb :bind "get_dry" :hash
  1740695150)
 float)

(defgmethod
 (audio-effect-reverb+set-wet :class 'audio-effect-reverb :bind "set_wet" :hash
  373806689)
 :void (amount float))

(defgmethod
 (audio-effect-reverb+get-wet :class 'audio-effect-reverb :bind "get_wet" :hash
  1740695150)
 float)

(defgmethod
 (audio-effect-reverb+set-hpf :class 'audio-effect-reverb :bind "set_hpf" :hash
  373806689)
 :void (amount float))

(defgmethod
 (audio-effect-reverb+get-hpf :class 'audio-effect-reverb :bind "get_hpf" :hash
  1740695150)
 float)