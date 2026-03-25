(common-lisp:in-package :%godot)


(defgmethod
 (rdshader-spirv+set-stage-bytecode :class 'rdshader-spirv :bind
  "set_stage_bytecode" :hash 3514097977)
 :void (stage rendering-device+shader-stage) (bytecode packed-byte-array))

(defgmethod
 (rdshader-spirv+get-stage-bytecode :class 'rdshader-spirv :bind
  "get_stage_bytecode" :hash 3816765404)
 packed-byte-array (stage rendering-device+shader-stage))

(defgmethod
 (rdshader-spirv+set-stage-compile-error :class 'rdshader-spirv :bind
  "set_stage_compile_error" :hash 620821314)
 :void (stage rendering-device+shader-stage) (compile-error string))

(defgmethod
 (rdshader-spirv+get-stage-compile-error :class 'rdshader-spirv :bind
  "get_stage_compile_error" :hash 3354920045)
 string (stage rendering-device+shader-stage))