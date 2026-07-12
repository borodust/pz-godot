(common-lisp:in-package :%godot)


(defgproperty option-button+selected 'option-button :get
 'option-button+get-selected)

(defgproperty option-button+fit-to-longest-item 'option-button :get
 'option-button+is-fit-to-longest-item :set
 'option-button+set-fit-to-longest-item)

(defgproperty option-button+allow-reselect 'option-button :get
 'option-button+get-allow-reselect :set 'option-button+set-allow-reselect)

(defgproperty option-button+search-bar-enabled 'option-button :get
 'option-button+is-search-bar-enabled :set
 'option-button+set-search-bar-enabled)

(defgproperty option-button+search-bar-min-item-count 'option-button :get
 'option-button+get-search-bar-min-item-count :set
 'option-button+set-search-bar-min-item-count)

(defgproperty option-button+search-bar-fuzzy-search-enabled 'option-button :get
 'option-button+is-search-bar-fuzzy-search-enabled :set
 'option-button+set-search-bar-fuzzy-search-enabled)

(defgproperty option-button+search-bar-fuzzy-search-max-misses 'option-button
 :get 'option-button+get-search-bar-fuzzy-search-max-misses :set
 'option-button+set-search-bar-fuzzy-search-max-misses)

(defgproperty option-button+item-count 'option-button :get
 'option-button+get-item-count :set 'option-button+set-item-count)