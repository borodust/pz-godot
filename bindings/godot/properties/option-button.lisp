(common-lisp:in-package :%godot)


(defgproperty option-button+selected 'option-button :get
 'option-button+get-selected)

(defgproperty option-button+fit-to-longest-item 'option-button :get
 'option-button+is-fit-to-longest-item :set
 'option-button+set-fit-to-longest-item)

(defgproperty option-button+allow-reselect 'option-button :get
 'option-button+get-allow-reselect :set 'option-button+set-allow-reselect)

(defgproperty option-button+item-count 'option-button :get
 'option-button+get-item-count :set 'option-button+set-item-count)