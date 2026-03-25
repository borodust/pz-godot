(common-lisp:in-package :%godot)


(defgproperty atlas-texture+atlas 'atlas-texture :get 'atlas-texture+get-atlas
 :set 'atlas-texture+set-atlas)

(defgproperty atlas-texture+region 'atlas-texture :get
 'atlas-texture+get-region :set 'atlas-texture+set-region)

(defgproperty atlas-texture+margin 'atlas-texture :get
 'atlas-texture+get-margin :set 'atlas-texture+set-margin)

(defgproperty atlas-texture+filter-clip 'atlas-texture :get
 'atlas-texture+has-filter-clip :set 'atlas-texture+set-filter-clip)