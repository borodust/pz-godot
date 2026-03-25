(common-lisp:in-package :%godot)


(defgproperty world-environment+environment 'world-environment :get
 'world-environment+get-environment :set 'world-environment+set-environment)

(defgproperty world-environment+camera-attributes 'world-environment :get
 'world-environment+get-camera-attributes :set
 'world-environment+set-camera-attributes)

(defgproperty world-environment+compositor 'world-environment :get
 'world-environment+get-compositor :set 'world-environment+set-compositor)