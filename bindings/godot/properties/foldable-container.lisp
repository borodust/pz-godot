(common-lisp:in-package :%godot)


(defgproperty foldable-container+folded 'foldable-container :get
 'foldable-container+is-folded :set 'foldable-container+set-folded)

(defgproperty foldable-container+title 'foldable-container :get
 'foldable-container+get-title :set 'foldable-container+set-title)

(defgproperty foldable-container+title-alignment 'foldable-container :get
 'foldable-container+get-title-alignment :set
 'foldable-container+set-title-alignment)

(defgproperty foldable-container+title-position 'foldable-container :get
 'foldable-container+get-title-position :set
 'foldable-container+set-title-position)

(defgproperty foldable-container+title-text-overrun-behavior
 'foldable-container :get 'foldable-container+get-title-text-overrun-behavior
 :set 'foldable-container+set-title-text-overrun-behavior)

(defgproperty foldable-container+foldable-group 'foldable-container :get
 'foldable-container+get-foldable-group :set
 'foldable-container+set-foldable-group)

(defgproperty foldable-container+title-text-direction 'foldable-container :get
 'foldable-container+get-title-text-direction :set
 'foldable-container+set-title-text-direction)

(defgproperty foldable-container+language 'foldable-container :get
 'foldable-container+get-language :set 'foldable-container+set-language)