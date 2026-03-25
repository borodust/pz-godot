(common-lisp:in-package :%godot)


(defgproperty style-box+content-margin-left 'style-box :index 0 :get
 'style-box+get-content-margin :set 'style-box+set-content-margin)

(defgproperty style-box+content-margin-top 'style-box :index 1 :get
 'style-box+get-content-margin :set 'style-box+set-content-margin)

(defgproperty style-box+content-margin-right 'style-box :index 2 :get
 'style-box+get-content-margin :set 'style-box+set-content-margin)

(defgproperty style-box+content-margin-bottom 'style-box :index 3 :get
 'style-box+get-content-margin :set 'style-box+set-content-margin)