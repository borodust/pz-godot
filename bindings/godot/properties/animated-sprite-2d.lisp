(common-lisp:in-package :%godot)


(defgproperty animated-sprite-2d+sprite-frames 'animated-sprite-2d :get
 'animated-sprite-2d+get-sprite-frames :set
 'animated-sprite-2d+set-sprite-frames)

(defgproperty animated-sprite-2d+animation 'animated-sprite-2d :get
 'animated-sprite-2d+get-animation :set 'animated-sprite-2d+set-animation)

(defgproperty animated-sprite-2d+autoplay 'animated-sprite-2d :get
 'animated-sprite-2d+get-autoplay :set 'animated-sprite-2d+set-autoplay)

(defgproperty animated-sprite-2d+frame 'animated-sprite-2d :get
 'animated-sprite-2d+get-frame :set 'animated-sprite-2d+set-frame)

(defgproperty animated-sprite-2d+frame-progress 'animated-sprite-2d :get
 'animated-sprite-2d+get-frame-progress :set
 'animated-sprite-2d+set-frame-progress)

(defgproperty animated-sprite-2d+speed-scale 'animated-sprite-2d :get
 'animated-sprite-2d+get-speed-scale :set 'animated-sprite-2d+set-speed-scale)

(defgproperty animated-sprite-2d+centered 'animated-sprite-2d :get
 'animated-sprite-2d+is-centered :set 'animated-sprite-2d+set-centered)

(defgproperty animated-sprite-2d+offset 'animated-sprite-2d :get
 'animated-sprite-2d+get-offset :set 'animated-sprite-2d+set-offset)

(defgproperty animated-sprite-2d+flip-h 'animated-sprite-2d :get
 'animated-sprite-2d+is-flipped-h :set 'animated-sprite-2d+set-flip-h)

(defgproperty animated-sprite-2d+flip-v 'animated-sprite-2d :get
 'animated-sprite-2d+is-flipped-v :set 'animated-sprite-2d+set-flip-v)