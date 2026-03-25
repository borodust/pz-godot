(common-lisp:in-package :%godot)


(defgproperty path-follow-2d+progress 'path-follow-2d :get
 'path-follow-2d+get-progress :set 'path-follow-2d+set-progress)

(defgproperty path-follow-2d+progress-ratio 'path-follow-2d :get
 'path-follow-2d+get-progress-ratio :set 'path-follow-2d+set-progress-ratio)

(defgproperty path-follow-2d+h-offset 'path-follow-2d :get
 'path-follow-2d+get-h-offset :set 'path-follow-2d+set-h-offset)

(defgproperty path-follow-2d+v-offset 'path-follow-2d :get
 'path-follow-2d+get-v-offset :set 'path-follow-2d+set-v-offset)

(defgproperty path-follow-2d+rotates 'path-follow-2d :get
 'path-follow-2d+is-rotating :set 'path-follow-2d+set-rotates)

(defgproperty path-follow-2d+cubic-interp 'path-follow-2d :get
 'path-follow-2d+get-cubic-interpolation :set
 'path-follow-2d+set-cubic-interpolation)

(defgproperty path-follow-2d+loop 'path-follow-2d :get 'path-follow-2d+has-loop
 :set 'path-follow-2d+set-loop)