(common-lisp:in-package :%godot)


(defgproperty httpclient+blocking-mode-enabled 'httpclient :get
 'httpclient+is-blocking-mode-enabled :set 'httpclient+set-blocking-mode)

(defgproperty httpclient+connection 'httpclient :get 'httpclient+get-connection
 :set 'httpclient+set-connection)

(defgproperty httpclient+read-chunk-size 'httpclient :get
 'httpclient+get-read-chunk-size :set 'httpclient+set-read-chunk-size)