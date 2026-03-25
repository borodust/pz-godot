(common-lisp:in-package :%godot)


(defgproperty animated-sprite-3d+sprite-frames 'animated-sprite-3d :get
 'animated-sprite-3d+get-sprite-frames :set
 'animated-sprite-3d+set-sprite-frames)

(defgproperty animated-sprite-3d+animation 'animated-sprite-3d :get
 'animated-sprite-3d+get-animation :set 'animated-sprite-3d+set-animation)

(defgproperty animated-sprite-3d+autoplay 'animated-sprite-3d :get
 'animated-sprite-3d+get-autoplay :set 'animated-sprite-3d+set-autoplay)

(defgproperty animated-sprite-3d+frame 'animated-sprite-3d :get
 'animated-sprite-3d+get-frame :set 'animated-sprite-3d+set-frame)

(defgproperty animated-sprite-3d+frame-progress 'animated-sprite-3d :get
 'animated-sprite-3d+get-frame-progress :set
 'animated-sprite-3d+set-frame-progress)

(defgproperty animated-sprite-3d+speed-scale 'animated-sprite-3d :get
 'animated-sprite-3d+get-speed-scale :set 'animated-sprite-3d+set-speed-scale)