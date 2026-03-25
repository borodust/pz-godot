(common-lisp:in-package :%godot)


(defgmethod
 (damped-spring-joint-2d+set-length :class 'damped-spring-joint-2d :bind
  "set_length" :hash 373806689)
 :void (length float))

(defgmethod
 (damped-spring-joint-2d+get-length :class 'damped-spring-joint-2d :bind
  "get_length" :hash 1740695150)
 float)

(defgmethod
 (damped-spring-joint-2d+set-rest-length :class 'damped-spring-joint-2d :bind
  "set_rest_length" :hash 373806689)
 :void (rest-length float))

(defgmethod
 (damped-spring-joint-2d+get-rest-length :class 'damped-spring-joint-2d :bind
  "get_rest_length" :hash 1740695150)
 float)

(defgmethod
 (damped-spring-joint-2d+set-stiffness :class 'damped-spring-joint-2d :bind
  "set_stiffness" :hash 373806689)
 :void (stiffness float))

(defgmethod
 (damped-spring-joint-2d+get-stiffness :class 'damped-spring-joint-2d :bind
  "get_stiffness" :hash 1740695150)
 float)

(defgmethod
 (damped-spring-joint-2d+set-damping :class 'damped-spring-joint-2d :bind
  "set_damping" :hash 373806689)
 :void (damping float))

(defgmethod
 (damped-spring-joint-2d+get-damping :class 'damped-spring-joint-2d :bind
  "get_damping" :hash 1740695150)
 float)