(common-lisp:in-package :%godot)


(defgproperty animated-texture+frames 'animated-texture :get
 'animated-texture+get-frames :set 'animated-texture+set-frames)

(defgproperty animated-texture+current-frame 'animated-texture :get
 'animated-texture+get-current-frame :set 'animated-texture+set-current-frame)

(defgproperty animated-texture+pause 'animated-texture :get
 'animated-texture+get-pause :set 'animated-texture+set-pause)

(defgproperty animated-texture+one-shot 'animated-texture :get
 'animated-texture+get-one-shot :set 'animated-texture+set-one-shot)

(defgproperty animated-texture+speed-scale 'animated-texture :get
 'animated-texture+get-speed-scale :set 'animated-texture+set-speed-scale)