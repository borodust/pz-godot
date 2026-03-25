(common-lisp:in-package :%godot)


(defgproperty visible-on-screen-notifier-2d+rect 'visible-on-screen-notifier-2d
 :get 'visible-on-screen-notifier-2d+get-rect :set
 'visible-on-screen-notifier-2d+set-rect)

(defgproperty visible-on-screen-notifier-2d+show-rect
 'visible-on-screen-notifier-2d :get
 'visible-on-screen-notifier-2d+is-showing-rect :set
 'visible-on-screen-notifier-2d+set-show-rect)