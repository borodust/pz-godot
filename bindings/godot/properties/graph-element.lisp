(common-lisp:in-package :%godot)


(defgproperty graph-element+position-offset 'graph-element :get
 'graph-element+get-position-offset :set 'graph-element+set-position-offset)

(defgproperty graph-element+resizable 'graph-element :get
 'graph-element+is-resizable :set 'graph-element+set-resizable)

(defgproperty graph-element+draggable 'graph-element :get
 'graph-element+is-draggable :set 'graph-element+set-draggable)

(defgproperty graph-element+selectable 'graph-element :get
 'graph-element+is-selectable :set 'graph-element+set-selectable)

(defgproperty graph-element+selected 'graph-element :get
 'graph-element+is-selected :set 'graph-element+set-selected)

(defgproperty graph-element+scaling-menus 'graph-element :get
 'graph-element+is-scaling-menus :set 'graph-element+set-scaling-menus)