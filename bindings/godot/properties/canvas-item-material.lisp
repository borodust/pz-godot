(common-lisp:in-package :%godot)


(defgproperty canvas-item-material+blend-mode 'canvas-item-material :get
 'canvas-item-material+get-blend-mode :set 'canvas-item-material+set-blend-mode)

(defgproperty canvas-item-material+light-mode 'canvas-item-material :get
 'canvas-item-material+get-light-mode :set 'canvas-item-material+set-light-mode)

(defgproperty canvas-item-material+particles-animation 'canvas-item-material
 :get 'canvas-item-material+get-particles-animation :set
 'canvas-item-material+set-particles-animation)

(defgproperty canvas-item-material+particles-anim-h-frames
 'canvas-item-material :get 'canvas-item-material+get-particles-anim-h-frames
 :set 'canvas-item-material+set-particles-anim-h-frames)

(defgproperty canvas-item-material+particles-anim-v-frames
 'canvas-item-material :get 'canvas-item-material+get-particles-anim-v-frames
 :set 'canvas-item-material+set-particles-anim-v-frames)

(defgproperty canvas-item-material+particles-anim-loop 'canvas-item-material
 :get 'canvas-item-material+get-particles-anim-loop :set
 'canvas-item-material+set-particles-anim-loop)