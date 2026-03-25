(common-lisp:in-package :%godot)


(defgproperty audio-effect-reverb+predelay-msec 'audio-effect-reverb :get
 'audio-effect-reverb+get-predelay-msec :set
 'audio-effect-reverb+set-predelay-msec)

(defgproperty audio-effect-reverb+predelay-feedback 'audio-effect-reverb :get
 'audio-effect-reverb+get-predelay-feedback :set
 'audio-effect-reverb+set-predelay-feedback)

(defgproperty audio-effect-reverb+room-size 'audio-effect-reverb :get
 'audio-effect-reverb+get-room-size :set 'audio-effect-reverb+set-room-size)

(defgproperty audio-effect-reverb+damping 'audio-effect-reverb :get
 'audio-effect-reverb+get-damping :set 'audio-effect-reverb+set-damping)

(defgproperty audio-effect-reverb+spread 'audio-effect-reverb :get
 'audio-effect-reverb+get-spread :set 'audio-effect-reverb+set-spread)

(defgproperty audio-effect-reverb+hipass 'audio-effect-reverb :get
 'audio-effect-reverb+get-hpf :set 'audio-effect-reverb+set-hpf)

(defgproperty audio-effect-reverb+dry 'audio-effect-reverb :get
 'audio-effect-reverb+get-dry :set 'audio-effect-reverb+set-dry)

(defgproperty audio-effect-reverb+wet 'audio-effect-reverb :get
 'audio-effect-reverb+get-wet :set 'audio-effect-reverb+set-wet)