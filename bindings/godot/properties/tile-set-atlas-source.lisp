(common-lisp:in-package :%godot)


(defgproperty tile-set-atlas-source+texture 'tile-set-atlas-source :get
 'tile-set-atlas-source+get-texture :set 'tile-set-atlas-source+set-texture)

(defgproperty tile-set-atlas-source+margins 'tile-set-atlas-source :get
 'tile-set-atlas-source+get-margins :set 'tile-set-atlas-source+set-margins)

(defgproperty tile-set-atlas-source+separation 'tile-set-atlas-source :get
 'tile-set-atlas-source+get-separation :set
 'tile-set-atlas-source+set-separation)

(defgproperty tile-set-atlas-source+texture-region-size 'tile-set-atlas-source
 :get 'tile-set-atlas-source+get-texture-region-size :set
 'tile-set-atlas-source+set-texture-region-size)

(defgproperty tile-set-atlas-source+use-texture-padding 'tile-set-atlas-source
 :get 'tile-set-atlas-source+get-use-texture-padding :set
 'tile-set-atlas-source+set-use-texture-padding)