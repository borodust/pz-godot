(common-lisp:in-package :%godot)


(defgmethod
 (rdpipeline-shader+set-shader :class 'rdpipeline-shader :bind "set_shader"
  :hash 2722037293)
 :void (p-member rid))

(defgmethod
 (rdpipeline-shader+get-shader :class 'rdpipeline-shader :bind "get_shader"
  :hash 2944877500)
 rid)

(defgmethod
 (rdpipeline-shader+set-specialization-constants :class 'rdpipeline-shader
  :bind "set_specialization_constants" :hash 381264803)
 :void (specialization-constants array))

(defgmethod
 (rdpipeline-shader+get-specialization-constants :class 'rdpipeline-shader
  :bind "get_specialization_constants" :hash 3995934104)
 array)