(common-lisp:in-package :%godot)


(defgproperty tab-container+tab-alignment 'tab-container :get
 'tab-container+get-tab-alignment :set 'tab-container+set-tab-alignment)

(defgproperty tab-container+current-tab 'tab-container :get
 'tab-container+get-current-tab :set 'tab-container+set-current-tab)

(defgproperty tab-container+tabs-position 'tab-container :get
 'tab-container+get-tabs-position :set 'tab-container+set-tabs-position)

(defgproperty tab-container+clip-tabs 'tab-container :get
 'tab-container+get-clip-tabs :set 'tab-container+set-clip-tabs)

(defgproperty tab-container+tabs-visible 'tab-container :get
 'tab-container+are-tabs-visible :set 'tab-container+set-tabs-visible)

(defgproperty tab-container+all-tabs-in-front 'tab-container :get
 'tab-container+is-all-tabs-in-front :set 'tab-container+set-all-tabs-in-front)

(defgproperty tab-container+switch-on-drag-hover 'tab-container :get
 'tab-container+get-switch-on-drag-hover :set
 'tab-container+set-switch-on-drag-hover)

(defgproperty tab-container+drag-to-rearrange-enabled 'tab-container :get
 'tab-container+get-drag-to-rearrange-enabled :set
 'tab-container+set-drag-to-rearrange-enabled)

(defgproperty tab-container+tabs-rearrange-group 'tab-container :get
 'tab-container+get-tabs-rearrange-group :set
 'tab-container+set-tabs-rearrange-group)

(defgproperty tab-container+use-hidden-tabs-for-min-size 'tab-container :get
 'tab-container+get-use-hidden-tabs-for-min-size :set
 'tab-container+set-use-hidden-tabs-for-min-size)

(defgproperty tab-container+tab-focus-mode 'tab-container :get
 'tab-container+get-tab-focus-mode :set 'tab-container+set-tab-focus-mode)

(defgproperty tab-container+deselect-enabled 'tab-container :get
 'tab-container+get-deselect-enabled :set 'tab-container+set-deselect-enabled)