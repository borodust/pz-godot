(common-lisp:in-package :%godot)


(defgproperty physical-bone-2d+bone2d-nodepath 'physical-bone-2d :get
 'physical-bone-2d+get-bone2d-nodepath :set
 'physical-bone-2d+set-bone2d-nodepath)

(defgproperty physical-bone-2d+bone2d-index 'physical-bone-2d :get
 'physical-bone-2d+get-bone2d-index :set 'physical-bone-2d+set-bone2d-index)

(defgproperty physical-bone-2d+auto-configure-joint 'physical-bone-2d :get
 'physical-bone-2d+get-auto-configure-joint :set
 'physical-bone-2d+set-auto-configure-joint)

(defgproperty physical-bone-2d+simulate-physics 'physical-bone-2d :get
 'physical-bone-2d+get-simulate-physics :set
 'physical-bone-2d+set-simulate-physics)

(defgproperty physical-bone-2d+follow-bone-when-simulating 'physical-bone-2d
 :get 'physical-bone-2d+get-follow-bone-when-simulating :set
 'physical-bone-2d+set-follow-bone-when-simulating)