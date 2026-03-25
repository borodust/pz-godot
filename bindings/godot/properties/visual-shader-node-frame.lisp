(common-lisp:in-package :%godot)


(defgproperty visual-shader-node-frame+title 'visual-shader-node-frame :get
 'visual-shader-node-frame+get-title :set 'visual-shader-node-frame+set-title)

(defgproperty visual-shader-node-frame+tint-color-enabled
 'visual-shader-node-frame :get 'visual-shader-node-frame+is-tint-color-enabled
 :set 'visual-shader-node-frame+set-tint-color-enabled)

(defgproperty visual-shader-node-frame+tint-color 'visual-shader-node-frame
 :get 'visual-shader-node-frame+get-tint-color :set
 'visual-shader-node-frame+set-tint-color)

(defgproperty visual-shader-node-frame+autoshrink 'visual-shader-node-frame
 :get 'visual-shader-node-frame+is-autoshrink-enabled :set
 'visual-shader-node-frame+set-autoshrink-enabled)

(defgproperty visual-shader-node-frame+attached-nodes 'visual-shader-node-frame
 :get 'visual-shader-node-frame+get-attached-nodes :set
 'visual-shader-node-frame+set-attached-nodes)