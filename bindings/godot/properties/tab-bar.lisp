(common-lisp:in-package :%godot)


(defgproperty tab-bar+current-tab 'tab-bar :get 'tab-bar+get-current-tab :set
 'tab-bar+set-current-tab)

(defgproperty tab-bar+tab-alignment 'tab-bar :get 'tab-bar+get-tab-alignment
 :set 'tab-bar+set-tab-alignment)

(defgproperty tab-bar+clip-tabs 'tab-bar :get 'tab-bar+get-clip-tabs :set
 'tab-bar+set-clip-tabs)

(defgproperty tab-bar+close-with-middle-mouse 'tab-bar :get
 'tab-bar+get-close-with-middle-mouse :set 'tab-bar+set-close-with-middle-mouse)

(defgproperty tab-bar+tab-close-display-policy 'tab-bar :get
 'tab-bar+get-tab-close-display-policy :set
 'tab-bar+set-tab-close-display-policy)

(defgproperty tab-bar+max-tab-width 'tab-bar :get 'tab-bar+get-max-tab-width
 :set 'tab-bar+set-max-tab-width)

(defgproperty tab-bar+scrolling-enabled 'tab-bar :get
 'tab-bar+get-scrolling-enabled :set 'tab-bar+set-scrolling-enabled)

(defgproperty tab-bar+drag-to-rearrange-enabled 'tab-bar :get
 'tab-bar+get-drag-to-rearrange-enabled :set
 'tab-bar+set-drag-to-rearrange-enabled)

(defgproperty tab-bar+switch-on-drag-hover 'tab-bar :get
 'tab-bar+get-switch-on-drag-hover :set 'tab-bar+set-switch-on-drag-hover)

(defgproperty tab-bar+tabs-rearrange-group 'tab-bar :get
 'tab-bar+get-tabs-rearrange-group :set 'tab-bar+set-tabs-rearrange-group)

(defgproperty tab-bar+scroll-to-selected 'tab-bar :get
 'tab-bar+get-scroll-to-selected :set 'tab-bar+set-scroll-to-selected)

(defgproperty tab-bar+select-with-rmb 'tab-bar :get
 'tab-bar+get-select-with-rmb :set 'tab-bar+set-select-with-rmb)

(defgproperty tab-bar+deselect-enabled 'tab-bar :get
 'tab-bar+get-deselect-enabled :set 'tab-bar+set-deselect-enabled)

(defgproperty tab-bar+tab-count 'tab-bar :get 'tab-bar+get-tab-count :set
 'tab-bar+set-tab-count)