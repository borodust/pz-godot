(common-lisp:in-package :%godot)


(defgproperty accept-dialog+ok-button-text 'accept-dialog :get
 'accept-dialog+get-ok-button-text :set 'accept-dialog+set-ok-button-text)

(defgproperty accept-dialog+dialog-text 'accept-dialog :get
 'accept-dialog+get-text :set 'accept-dialog+set-text)

(defgproperty accept-dialog+dialog-hide-on-ok 'accept-dialog :get
 'accept-dialog+get-hide-on-ok :set 'accept-dialog+set-hide-on-ok)

(defgproperty accept-dialog+dialog-close-on-escape 'accept-dialog :get
 'accept-dialog+get-close-on-escape :set 'accept-dialog+set-close-on-escape)

(defgproperty accept-dialog+dialog-autowrap 'accept-dialog :get
 'accept-dialog+has-autowrap :set 'accept-dialog+set-autowrap)