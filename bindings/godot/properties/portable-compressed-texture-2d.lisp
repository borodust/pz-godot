(common-lisp:in-package :%godot)


(defgproperty portable-compressed-texture-2d+size-override
 'portable-compressed-texture-2d :get
 'portable-compressed-texture-2d+get-size-override :set
 'portable-compressed-texture-2d+set-size-override)

(defgproperty portable-compressed-texture-2d+keep-compressed-buffer
 'portable-compressed-texture-2d :get
 'portable-compressed-texture-2d+is-keeping-compressed-buffer :set
 'portable-compressed-texture-2d+set-keep-compressed-buffer)