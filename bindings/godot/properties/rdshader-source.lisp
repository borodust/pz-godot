(common-lisp:in-package :%godot)


(defgproperty rdshader-source+source-vertex 'rdshader-source :index 0 :get
 'rdshader-source+get-stage-source :set 'rdshader-source+set-stage-source)

(defgproperty rdshader-source+source-fragment 'rdshader-source :index 1 :get
 'rdshader-source+get-stage-source :set 'rdshader-source+set-stage-source)

(defgproperty rdshader-source+source-tesselation-control 'rdshader-source
 :index 2 :get 'rdshader-source+get-stage-source :set
 'rdshader-source+set-stage-source)

(defgproperty rdshader-source+source-tesselation-evaluation 'rdshader-source
 :index 3 :get 'rdshader-source+get-stage-source :set
 'rdshader-source+set-stage-source)

(defgproperty rdshader-source+source-compute 'rdshader-source :index 4 :get
 'rdshader-source+get-stage-source :set 'rdshader-source+set-stage-source)

(defgproperty rdshader-source+source-raygen 'rdshader-source :index 5 :get
 'rdshader-source+get-stage-source :set 'rdshader-source+set-stage-source)

(defgproperty rdshader-source+source-any-hit 'rdshader-source :index 6 :get
 'rdshader-source+get-stage-source :set 'rdshader-source+set-stage-source)

(defgproperty rdshader-source+source-closest-hit 'rdshader-source :index 7 :get
 'rdshader-source+get-stage-source :set 'rdshader-source+set-stage-source)

(defgproperty rdshader-source+source-miss 'rdshader-source :index 8 :get
 'rdshader-source+get-stage-source :set 'rdshader-source+set-stage-source)

(defgproperty rdshader-source+source-intersection 'rdshader-source :index 9
 :get 'rdshader-source+get-stage-source :set 'rdshader-source+set-stage-source)

(defgproperty rdshader-source+language 'rdshader-source :get
 'rdshader-source+get-language :set 'rdshader-source+set-language)