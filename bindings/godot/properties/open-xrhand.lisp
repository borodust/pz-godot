(common-lisp:in-package :%godot)


(defgproperty open-xrhand+hand 'open-xrhand :get 'open-xrhand+get-hand :set
 'open-xrhand+set-hand)

(defgproperty open-xrhand+motion-range 'open-xrhand :get
 'open-xrhand+get-motion-range :set 'open-xrhand+set-motion-range)

(defgproperty open-xrhand+hand-skeleton 'open-xrhand :get
 'open-xrhand+get-hand-skeleton :set 'open-xrhand+set-hand-skeleton)

(defgproperty open-xrhand+skeleton-rig 'open-xrhand :get
 'open-xrhand+get-skeleton-rig :set 'open-xrhand+set-skeleton-rig)

(defgproperty open-xrhand+bone-update 'open-xrhand :get
 'open-xrhand+get-bone-update :set 'open-xrhand+set-bone-update)