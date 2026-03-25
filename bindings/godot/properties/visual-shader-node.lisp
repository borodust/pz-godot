(common-lisp:in-package :%godot)


(defgproperty visual-shader-node+output-port-for-preview 'visual-shader-node
 :get 'visual-shader-node+get-output-port-for-preview :set
 'visual-shader-node+set-output-port-for-preview)

(defgproperty visual-shader-node+default-input-values 'visual-shader-node :get
 'visual-shader-node+get-default-input-values :set
 'visual-shader-node+set-default-input-values)

(defgproperty visual-shader-node+expanded-output-ports 'visual-shader-node)

(defgproperty visual-shader-node+linked-parent-graph-frame 'visual-shader-node
 :get 'visual-shader-node+get-frame :set 'visual-shader-node+set-frame)