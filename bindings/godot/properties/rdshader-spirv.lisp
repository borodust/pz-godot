(common-lisp:in-package :%godot)


(defgproperty rdshader-spirv+bytecode-vertex 'rdshader-spirv :index 0 :get
 'rdshader-spirv+get-stage-bytecode :set 'rdshader-spirv+set-stage-bytecode)

(defgproperty rdshader-spirv+bytecode-fragment 'rdshader-spirv :index 1 :get
 'rdshader-spirv+get-stage-bytecode :set 'rdshader-spirv+set-stage-bytecode)

(defgproperty rdshader-spirv+bytecode-tesselation-control 'rdshader-spirv
 :index 2 :get 'rdshader-spirv+get-stage-bytecode :set
 'rdshader-spirv+set-stage-bytecode)

(defgproperty rdshader-spirv+bytecode-tesselation-evaluation 'rdshader-spirv
 :index 3 :get 'rdshader-spirv+get-stage-bytecode :set
 'rdshader-spirv+set-stage-bytecode)

(defgproperty rdshader-spirv+bytecode-compute 'rdshader-spirv :index 4 :get
 'rdshader-spirv+get-stage-bytecode :set 'rdshader-spirv+set-stage-bytecode)

(defgproperty rdshader-spirv+compile-error-vertex 'rdshader-spirv :index 0 :get
 'rdshader-spirv+get-stage-compile-error :set
 'rdshader-spirv+set-stage-compile-error)

(defgproperty rdshader-spirv+compile-error-fragment 'rdshader-spirv :index 1
 :get 'rdshader-spirv+get-stage-compile-error :set
 'rdshader-spirv+set-stage-compile-error)

(defgproperty rdshader-spirv+compile-error-tesselation-control 'rdshader-spirv
 :index 2 :get 'rdshader-spirv+get-stage-compile-error :set
 'rdshader-spirv+set-stage-compile-error)

(defgproperty rdshader-spirv+compile-error-tesselation-evaluation
 'rdshader-spirv :index 3 :get 'rdshader-spirv+get-stage-compile-error :set
 'rdshader-spirv+set-stage-compile-error)

(defgproperty rdshader-spirv+compile-error-compute 'rdshader-spirv :index 4
 :get 'rdshader-spirv+get-stage-compile-error :set
 'rdshader-spirv+set-stage-compile-error)