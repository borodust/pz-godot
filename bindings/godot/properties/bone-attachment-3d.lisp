(common-lisp:in-package :%godot)


(defgproperty bone-attachment-3d+bone-name 'bone-attachment-3d :get
 'bone-attachment-3d+get-bone-name :set 'bone-attachment-3d+set-bone-name)

(defgproperty bone-attachment-3d+bone-idx 'bone-attachment-3d :get
 'bone-attachment-3d+get-bone-idx :set 'bone-attachment-3d+set-bone-idx)

(defgproperty bone-attachment-3d+override-pose 'bone-attachment-3d :get
 'bone-attachment-3d+get-override-pose :set
 'bone-attachment-3d+set-override-pose)

(defgproperty bone-attachment-3d+use-external-skeleton 'bone-attachment-3d :get
 'bone-attachment-3d+get-use-external-skeleton :set
 'bone-attachment-3d+set-use-external-skeleton)

(defgproperty bone-attachment-3d+external-skeleton 'bone-attachment-3d :get
 'bone-attachment-3d+get-external-skeleton :set
 'bone-attachment-3d+set-external-skeleton)