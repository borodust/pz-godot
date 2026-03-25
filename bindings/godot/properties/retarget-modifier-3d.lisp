(common-lisp:in-package :%godot)


(defgproperty retarget-modifier-3d+profile 'retarget-modifier-3d :get
 'retarget-modifier-3d+get-profile :set 'retarget-modifier-3d+set-profile)

(defgproperty retarget-modifier-3d+use-global-pose 'retarget-modifier-3d :get
 'retarget-modifier-3d+is-using-global-pose :set
 'retarget-modifier-3d+set-use-global-pose)

(defgproperty retarget-modifier-3d+enable 'retarget-modifier-3d :get
 'retarget-modifier-3d+get-enable-flags :set
 'retarget-modifier-3d+set-enable-flags)