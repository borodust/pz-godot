(common-lisp:in-package :%godot)


(defgproperty line-2d+points 'line-2d :get 'line-2d+get-points :set
 'line-2d+set-points)

(defgproperty line-2d+closed 'line-2d :get 'line-2d+is-closed :set
 'line-2d+set-closed)

(defgproperty line-2d+width 'line-2d :get 'line-2d+get-width :set
 'line-2d+set-width)

(defgproperty line-2d+width-curve 'line-2d :get 'line-2d+get-curve :set
 'line-2d+set-curve)

(defgproperty line-2d+default-color 'line-2d :get 'line-2d+get-default-color
 :set 'line-2d+set-default-color)

(defgproperty line-2d+gradient 'line-2d :get 'line-2d+get-gradient :set
 'line-2d+set-gradient)

(defgproperty line-2d+texture 'line-2d :get 'line-2d+get-texture :set
 'line-2d+set-texture)

(defgproperty line-2d+texture-mode 'line-2d :get 'line-2d+get-texture-mode :set
 'line-2d+set-texture-mode)

(defgproperty line-2d+joint-mode 'line-2d :get 'line-2d+get-joint-mode :set
 'line-2d+set-joint-mode)

(defgproperty line-2d+begin-cap-mode 'line-2d :get 'line-2d+get-begin-cap-mode
 :set 'line-2d+set-begin-cap-mode)

(defgproperty line-2d+end-cap-mode 'line-2d :get 'line-2d+get-end-cap-mode :set
 'line-2d+set-end-cap-mode)

(defgproperty line-2d+sharp-limit 'line-2d :get 'line-2d+get-sharp-limit :set
 'line-2d+set-sharp-limit)

(defgproperty line-2d+round-precision 'line-2d :get
 'line-2d+get-round-precision :set 'line-2d+set-round-precision)

(defgproperty line-2d+antialiased 'line-2d :get 'line-2d+get-antialiased :set
 'line-2d+set-antialiased)