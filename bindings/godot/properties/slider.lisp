(common-lisp:in-package :%godot)


(defgproperty slider+editable 'slider :get 'slider+is-editable :set
 'slider+set-editable)

(defgproperty slider+scrollable 'slider :get 'slider+is-scrollable :set
 'slider+set-scrollable)

(defgproperty slider+tick-count 'slider :get 'slider+get-ticks :set
 'slider+set-ticks)

(defgproperty slider+ticks-on-borders 'slider :get 'slider+get-ticks-on-borders
 :set 'slider+set-ticks-on-borders)

(defgproperty slider+ticks-position 'slider :get 'slider+get-ticks-position
 :set 'slider+set-ticks-position)