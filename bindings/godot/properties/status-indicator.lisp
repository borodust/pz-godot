(common-lisp:in-package :%godot)


(defgproperty status-indicator+tooltip 'status-indicator :get
 'status-indicator+get-tooltip :set 'status-indicator+set-tooltip)

(defgproperty status-indicator+icon 'status-indicator :get
 'status-indicator+get-icon :set 'status-indicator+set-icon)

(defgproperty status-indicator+menu 'status-indicator :get
 'status-indicator+get-menu :set 'status-indicator+set-menu)

(defgproperty status-indicator+visible 'status-indicator :get
 'status-indicator+is-visible :set 'status-indicator+set-visible)