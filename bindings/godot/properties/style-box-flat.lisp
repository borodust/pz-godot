(common-lisp:in-package :%godot)


(defgproperty style-box-flat+bg-color 'style-box-flat :get
 'style-box-flat+get-bg-color :set 'style-box-flat+set-bg-color)

(defgproperty style-box-flat+draw-center 'style-box-flat :get
 'style-box-flat+is-draw-center-enabled :set 'style-box-flat+set-draw-center)

(defgproperty style-box-flat+skew 'style-box-flat :get 'style-box-flat+get-skew
 :set 'style-box-flat+set-skew)

(defgproperty style-box-flat+border-width-left 'style-box-flat :index 0 :get
 'style-box-flat+get-border-width :set 'style-box-flat+set-border-width)

(defgproperty style-box-flat+border-width-top 'style-box-flat :index 1 :get
 'style-box-flat+get-border-width :set 'style-box-flat+set-border-width)

(defgproperty style-box-flat+border-width-right 'style-box-flat :index 2 :get
 'style-box-flat+get-border-width :set 'style-box-flat+set-border-width)

(defgproperty style-box-flat+border-width-bottom 'style-box-flat :index 3 :get
 'style-box-flat+get-border-width :set 'style-box-flat+set-border-width)

(defgproperty style-box-flat+border-color 'style-box-flat :get
 'style-box-flat+get-border-color :set 'style-box-flat+set-border-color)

(defgproperty style-box-flat+border-blend 'style-box-flat :get
 'style-box-flat+get-border-blend :set 'style-box-flat+set-border-blend)

(defgproperty style-box-flat+corner-radius-top-left 'style-box-flat :index 0
 :get 'style-box-flat+get-corner-radius :set 'style-box-flat+set-corner-radius)

(defgproperty style-box-flat+corner-radius-top-right 'style-box-flat :index 1
 :get 'style-box-flat+get-corner-radius :set 'style-box-flat+set-corner-radius)

(defgproperty style-box-flat+corner-radius-bottom-right 'style-box-flat :index
 2 :get 'style-box-flat+get-corner-radius :set
 'style-box-flat+set-corner-radius)

(defgproperty style-box-flat+corner-radius-bottom-left 'style-box-flat :index 3
 :get 'style-box-flat+get-corner-radius :set 'style-box-flat+set-corner-radius)

(defgproperty style-box-flat+corner-detail 'style-box-flat :get
 'style-box-flat+get-corner-detail :set 'style-box-flat+set-corner-detail)

(defgproperty style-box-flat+expand-margin-left 'style-box-flat :index 0 :get
 'style-box-flat+get-expand-margin :set 'style-box-flat+set-expand-margin)

(defgproperty style-box-flat+expand-margin-top 'style-box-flat :index 1 :get
 'style-box-flat+get-expand-margin :set 'style-box-flat+set-expand-margin)

(defgproperty style-box-flat+expand-margin-right 'style-box-flat :index 2 :get
 'style-box-flat+get-expand-margin :set 'style-box-flat+set-expand-margin)

(defgproperty style-box-flat+expand-margin-bottom 'style-box-flat :index 3 :get
 'style-box-flat+get-expand-margin :set 'style-box-flat+set-expand-margin)

(defgproperty style-box-flat+shadow-color 'style-box-flat :get
 'style-box-flat+get-shadow-color :set 'style-box-flat+set-shadow-color)

(defgproperty style-box-flat+shadow-size 'style-box-flat :get
 'style-box-flat+get-shadow-size :set 'style-box-flat+set-shadow-size)

(defgproperty style-box-flat+shadow-offset 'style-box-flat :get
 'style-box-flat+get-shadow-offset :set 'style-box-flat+set-shadow-offset)

(defgproperty style-box-flat+anti-aliasing 'style-box-flat :get
 'style-box-flat+is-anti-aliased :set 'style-box-flat+set-anti-aliased)

(defgproperty style-box-flat+anti-aliasing-size 'style-box-flat :get
 'style-box-flat+get-aa-size :set 'style-box-flat+set-aa-size)