(common-lisp:in-package :%godot)


(defgproperty skeleton-profile+root-bone 'skeleton-profile :get
 'skeleton-profile+get-root-bone :set 'skeleton-profile+set-root-bone)

(defgproperty skeleton-profile+scale-base-bone 'skeleton-profile :get
 'skeleton-profile+get-scale-base-bone :set
 'skeleton-profile+set-scale-base-bone)

(defgproperty skeleton-profile+group-size 'skeleton-profile :get
 'skeleton-profile+get-group-size :set 'skeleton-profile+set-group-size)

(defgproperty skeleton-profile+bone-size 'skeleton-profile :get
 'skeleton-profile+get-bone-size :set 'skeleton-profile+set-bone-size)