(common-lisp:in-package :%godot)


(defgmethod
 (shader-material+set-shader :class 'shader-material :bind "set_shader" :hash
  3341921675)
 :void (shader shader))

(defgmethod
 (shader-material+get-shader :class 'shader-material :bind "get_shader" :hash
  2078273437)
 shader)

(defgmethod
 (shader-material+set-shader-parameter :class 'shader-material :bind
  "set_shader_parameter" :hash 3776071444)
 :void (param string-name) (value variant))

(defgmethod
 (shader-material+get-shader-parameter :class 'shader-material :bind
  "get_shader_parameter" :hash 2760726917)
 variant (param string-name))