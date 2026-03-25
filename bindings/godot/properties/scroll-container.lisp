(common-lisp:in-package :%godot)


(defgproperty scroll-container+follow-focus 'scroll-container :get
 'scroll-container+is-following-focus :set 'scroll-container+set-follow-focus)

(defgproperty scroll-container+draw-focus-border 'scroll-container :get
 'scroll-container+get-draw-focus-border :set
 'scroll-container+set-draw-focus-border)

(defgproperty scroll-container+scroll-horizontal 'scroll-container :get
 'scroll-container+get-h-scroll :set 'scroll-container+set-h-scroll)

(defgproperty scroll-container+scroll-vertical 'scroll-container :get
 'scroll-container+get-v-scroll :set 'scroll-container+set-v-scroll)

(defgproperty scroll-container+scroll-horizontal-custom-step 'scroll-container
 :get 'scroll-container+get-horizontal-custom-step :set
 'scroll-container+set-horizontal-custom-step)

(defgproperty scroll-container+scroll-vertical-custom-step 'scroll-container
 :get 'scroll-container+get-vertical-custom-step :set
 'scroll-container+set-vertical-custom-step)

(defgproperty scroll-container+horizontal-scroll-mode 'scroll-container :get
 'scroll-container+get-horizontal-scroll-mode :set
 'scroll-container+set-horizontal-scroll-mode)

(defgproperty scroll-container+vertical-scroll-mode 'scroll-container :get
 'scroll-container+get-vertical-scroll-mode :set
 'scroll-container+set-vertical-scroll-mode)

(defgproperty scroll-container+scroll-deadzone 'scroll-container :get
 'scroll-container+get-deadzone :set 'scroll-container+set-deadzone)

(defgproperty scroll-container+scroll-hint-mode 'scroll-container :get
 'scroll-container+get-scroll-hint-mode :set
 'scroll-container+set-scroll-hint-mode)

(defgproperty scroll-container+tile-scroll-hint 'scroll-container :get
 'scroll-container+is-scroll-hint-tiled :set
 'scroll-container+set-tile-scroll-hint)