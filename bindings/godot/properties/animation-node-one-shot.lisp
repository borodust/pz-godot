(common-lisp:in-package :%godot)


(defgproperty animation-node-one-shot+mix-mode 'animation-node-one-shot :get
 'animation-node-one-shot+get-mix-mode :set
 'animation-node-one-shot+set-mix-mode)

(defgproperty animation-node-one-shot+fadein-time 'animation-node-one-shot :get
 'animation-node-one-shot+get-fadein-time :set
 'animation-node-one-shot+set-fadein-time)

(defgproperty animation-node-one-shot+fadein-curve 'animation-node-one-shot
 :get 'animation-node-one-shot+get-fadein-curve :set
 'animation-node-one-shot+set-fadein-curve)

(defgproperty animation-node-one-shot+fadeout-time 'animation-node-one-shot
 :get 'animation-node-one-shot+get-fadeout-time :set
 'animation-node-one-shot+set-fadeout-time)

(defgproperty animation-node-one-shot+fadeout-curve 'animation-node-one-shot
 :get 'animation-node-one-shot+get-fadeout-curve :set
 'animation-node-one-shot+set-fadeout-curve)

(defgproperty animation-node-one-shot+break-loop-at-end
 'animation-node-one-shot :get 'animation-node-one-shot+is-loop-broken-at-end
 :set 'animation-node-one-shot+set-break-loop-at-end)

(defgproperty animation-node-one-shot+abort-on-reset 'animation-node-one-shot
 :get 'animation-node-one-shot+is-aborted-on-reset :set
 'animation-node-one-shot+set-abort-on-reset)

(defgproperty animation-node-one-shot+autorestart 'animation-node-one-shot :get
 'animation-node-one-shot+has-autorestart :set
 'animation-node-one-shot+set-autorestart)

(defgproperty animation-node-one-shot+autorestart-delay
 'animation-node-one-shot :get 'animation-node-one-shot+get-autorestart-delay
 :set 'animation-node-one-shot+set-autorestart-delay)

(defgproperty animation-node-one-shot+autorestart-random-delay
 'animation-node-one-shot :get
 'animation-node-one-shot+get-autorestart-random-delay :set
 'animation-node-one-shot+set-autorestart-random-delay)