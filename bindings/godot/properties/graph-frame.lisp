(common-lisp:in-package :%godot)


(defgproperty graph-frame+title 'graph-frame :get 'graph-frame+get-title :set
 'graph-frame+set-title)

(defgproperty graph-frame+autoshrink-enabled 'graph-frame :get
 'graph-frame+is-autoshrink-enabled :set 'graph-frame+set-autoshrink-enabled)

(defgproperty graph-frame+autoshrink-margin 'graph-frame :get
 'graph-frame+get-autoshrink-margin :set 'graph-frame+set-autoshrink-margin)

(defgproperty graph-frame+drag-margin 'graph-frame :get
 'graph-frame+get-drag-margin :set 'graph-frame+set-drag-margin)

(defgproperty graph-frame+tint-color-enabled 'graph-frame :get
 'graph-frame+is-tint-color-enabled :set 'graph-frame+set-tint-color-enabled)

(defgproperty graph-frame+tint-color 'graph-frame :get
 'graph-frame+get-tint-color :set 'graph-frame+set-tint-color)