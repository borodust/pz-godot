(common-lisp:in-package :%godot)


(defgproperty base-button+disabled 'base-button :get 'base-button+is-disabled
 :set 'base-button+set-disabled)

(defgproperty base-button+toggle-mode 'base-button :get
 'base-button+is-toggle-mode :set 'base-button+set-toggle-mode)

(defgproperty base-button+button-pressed 'base-button :get
 'base-button+is-pressed :set 'base-button+set-pressed)

(defgproperty base-button+action-mode 'base-button :get
 'base-button+get-action-mode :set 'base-button+set-action-mode)

(defgproperty base-button+button-mask 'base-button :get
 'base-button+get-button-mask :set 'base-button+set-button-mask)

(defgproperty base-button+keep-pressed-outside 'base-button :get
 'base-button+is-keep-pressed-outside :set
 'base-button+set-keep-pressed-outside)

(defgproperty base-button+button-group 'base-button :get
 'base-button+get-button-group :set 'base-button+set-button-group)

(defgproperty base-button+shortcut 'base-button :get 'base-button+get-shortcut
 :set 'base-button+set-shortcut)

(defgproperty base-button+shortcut-feedback 'base-button :get
 'base-button+is-shortcut-feedback :set 'base-button+set-shortcut-feedback)

(defgproperty base-button+shortcut-in-tooltip 'base-button :get
 'base-button+is-shortcut-in-tooltip-enabled :set
 'base-button+set-shortcut-in-tooltip)