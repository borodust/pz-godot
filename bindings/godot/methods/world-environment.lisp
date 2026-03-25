(common-lisp:in-package :%godot)


(defgmethod
 (world-environment+set-environment :class 'world-environment :bind
  "set_environment" :hash 4143518816)
 :void (env environment))

(defgmethod
 (world-environment+get-environment :class 'world-environment :bind
  "get_environment" :hash 3082064660)
 environment)

(defgmethod
 (world-environment+set-camera-attributes :class 'world-environment :bind
  "set_camera_attributes" :hash 2817810567)
 :void (camera-attributes camera-attributes))

(defgmethod
 (world-environment+get-camera-attributes :class 'world-environment :bind
  "get_camera_attributes" :hash 3921283215)
 camera-attributes)

(defgmethod
 (world-environment+set-compositor :class 'world-environment :bind
  "set_compositor" :hash 1586754307)
 :void (compositor compositor))

(defgmethod
 (world-environment+get-compositor :class 'world-environment :bind
  "get_compositor" :hash 3647707413)
 compositor)