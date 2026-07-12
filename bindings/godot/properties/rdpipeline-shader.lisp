(common-lisp:in-package :%godot)


(defgproperty rdpipeline-shader+shader 'rdpipeline-shader :get
 'rdpipeline-shader+get-shader :set 'rdpipeline-shader+set-shader)

(defgproperty rdpipeline-shader+specialization-constants 'rdpipeline-shader
 :get 'rdpipeline-shader+get-specialization-constants :set
 'rdpipeline-shader+set-specialization-constants)