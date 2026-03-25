(common-lisp:in-package :%godot)


(defgproperty fog-material+density 'fog-material :get 'fog-material+get-density
 :set 'fog-material+set-density)

(defgproperty fog-material+albedo 'fog-material :get 'fog-material+get-albedo
 :set 'fog-material+set-albedo)

(defgproperty fog-material+emission 'fog-material :get
 'fog-material+get-emission :set 'fog-material+set-emission)

(defgproperty fog-material+height-falloff 'fog-material :get
 'fog-material+get-height-falloff :set 'fog-material+set-height-falloff)

(defgproperty fog-material+edge-fade 'fog-material :get
 'fog-material+get-edge-fade :set 'fog-material+set-edge-fade)

(defgproperty fog-material+density-texture 'fog-material :get
 'fog-material+get-density-texture :set 'fog-material+set-density-texture)