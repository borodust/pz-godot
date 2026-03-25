(common-lisp:in-package :%godot)


(defgproperty damped-spring-joint-2d+length 'damped-spring-joint-2d :get
 'damped-spring-joint-2d+get-length :set 'damped-spring-joint-2d+set-length)

(defgproperty damped-spring-joint-2d+rest-length 'damped-spring-joint-2d :get
 'damped-spring-joint-2d+get-rest-length :set
 'damped-spring-joint-2d+set-rest-length)

(defgproperty damped-spring-joint-2d+stiffness 'damped-spring-joint-2d :get
 'damped-spring-joint-2d+get-stiffness :set
 'damped-spring-joint-2d+set-stiffness)

(defgproperty damped-spring-joint-2d+damping 'damped-spring-joint-2d :get
 'damped-spring-joint-2d+get-damping :set 'damped-spring-joint-2d+set-damping)