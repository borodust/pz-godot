(common-lisp:in-package :%godot)


(defgproperty skeleton-ik3d+root-bone 'skeleton-ik3d :get
 'skeleton-ik3d+get-root-bone :set 'skeleton-ik3d+set-root-bone)

(defgproperty skeleton-ik3d+tip-bone 'skeleton-ik3d :get
 'skeleton-ik3d+get-tip-bone :set 'skeleton-ik3d+set-tip-bone)

(defgproperty skeleton-ik3d+target 'skeleton-ik3d :get
 'skeleton-ik3d+get-target-transform :set 'skeleton-ik3d+set-target-transform)

(defgproperty skeleton-ik3d+override-tip-basis 'skeleton-ik3d :get
 'skeleton-ik3d+is-override-tip-basis :set
 'skeleton-ik3d+set-override-tip-basis)

(defgproperty skeleton-ik3d+use-magnet 'skeleton-ik3d :get
 'skeleton-ik3d+is-using-magnet :set 'skeleton-ik3d+set-use-magnet)

(defgproperty skeleton-ik3d+magnet 'skeleton-ik3d :get
 'skeleton-ik3d+get-magnet-position :set 'skeleton-ik3d+set-magnet-position)

(defgproperty skeleton-ik3d+target-node 'skeleton-ik3d :get
 'skeleton-ik3d+get-target-node :set 'skeleton-ik3d+set-target-node)

(defgproperty skeleton-ik3d+min-distance 'skeleton-ik3d :get
 'skeleton-ik3d+get-min-distance :set 'skeleton-ik3d+set-min-distance)

(defgproperty skeleton-ik3d+max-iterations 'skeleton-ik3d :get
 'skeleton-ik3d+get-max-iterations :set 'skeleton-ik3d+set-max-iterations)

(defgproperty skeleton-ik3d+interpolation 'skeleton-ik3d :get
 'skeleton-ik3d+get-interpolation :set 'skeleton-ik3d+set-interpolation)