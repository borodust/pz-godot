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

(defgproperty rdshader-spirv+bytecode-raygen 'rdshader-spirv :index 5 :get
 'rdshader-spirv+get-stage-bytecode :set 'rdshader-spirv+set-stage-bytecode)

(defgproperty rdshader-spirv+bytecode-any-hit 'rdshader-spirv :index 6 :get
 'rdshader-spirv+get-stage-bytecode :set 'rdshader-spirv+set-stage-bytecode)

(defgproperty rdshader-spirv+bytecode-closest-hit 'rdshader-spirv :index 7 :get
 'rdshader-spirv+get-stage-bytecode :set 'rdshader-spirv+set-stage-bytecode)

(defgproperty rdshader-spirv+bytecode-miss 'rdshader-spirv :index 8 :get
 'rdshader-spirv+get-stage-bytecode :set 'rdshader-spirv+set-stage-bytecode)

(defgproperty rdshader-spirv+bytecode-intersection 'rdshader-spirv :index 9
 :get 'rdshader-spirv+get-stage-bytecode :set
 'rdshader-spirv+set-stage-bytecode)

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

(defgproperty rdshader-spirv+compile-error-raygen 'rdshader-spirv :index 5 :get
 'rdshader-spirv+get-stage-compile-error :set
 'rdshader-spirv+set-stage-compile-error)

(defgproperty rdshader-spirv+compile-error-any-hit 'rdshader-spirv :index 6
 :get 'rdshader-spirv+get-stage-compile-error :set
 'rdshader-spirv+set-stage-compile-error)

(defgproperty rdshader-spirv+compile-error-closest-hit 'rdshader-spirv :index 7
 :get 'rdshader-spirv+get-stage-compile-error :set
 'rdshader-spirv+set-stage-compile-error)

(defgproperty rdshader-spirv+compile-error-miss 'rdshader-spirv :index 8 :get
 'rdshader-spirv+get-stage-compile-error :set
 'rdshader-spirv+set-stage-compile-error)

(defgproperty rdshader-spirv+compile-error-intersection 'rdshader-spirv :index
 9 :get 'rdshader-spirv+get-stage-compile-error :set
 'rdshader-spirv+set-stage-compile-error)