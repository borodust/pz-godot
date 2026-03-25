(common-lisp:in-package :%godot)


(defgproperty spring-bone-simulator-3d+external-force 'spring-bone-simulator-3d
 :get 'spring-bone-simulator-3d+get-external-force :set
 'spring-bone-simulator-3d+set-external-force)

(defgproperty spring-bone-simulator-3d+mutable-bone-axes
 'spring-bone-simulator-3d :get 'spring-bone-simulator-3d+are-bone-axes-mutable
 :set 'spring-bone-simulator-3d+set-mutable-bone-axes)

(defgproperty spring-bone-simulator-3d+setting-count 'spring-bone-simulator-3d
 :get 'spring-bone-simulator-3d+get-setting-count :set
 'spring-bone-simulator-3d+set-setting-count)