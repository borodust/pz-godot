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

(defgproperty rdshader-source+language 'rdshader-source :get
 'rdshader-source+get-language :set 'rdshader-source+set-language)