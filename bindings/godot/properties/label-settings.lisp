(common-lisp:in-package :%godot)


(defgproperty label-settings+line-spacing 'label-settings :get
 'label-settings+get-line-spacing :set 'label-settings+set-line-spacing)

(defgproperty label-settings+paragraph-spacing 'label-settings :get
 'label-settings+get-paragraph-spacing :set
 'label-settings+set-paragraph-spacing)

(defgproperty label-settings+font 'label-settings :get 'label-settings+get-font
 :set 'label-settings+set-font)

(defgproperty label-settings+font-size 'label-settings :get
 'label-settings+get-font-size :set 'label-settings+set-font-size)

(defgproperty label-settings+font-color 'label-settings :get
 'label-settings+get-font-color :set 'label-settings+set-font-color)

(defgproperty label-settings+outline-size 'label-settings :get
 'label-settings+get-outline-size :set 'label-settings+set-outline-size)

(defgproperty label-settings+outline-color 'label-settings :get
 'label-settings+get-outline-color :set 'label-settings+set-outline-color)

(defgproperty label-settings+shadow-size 'label-settings :get
 'label-settings+get-shadow-size :set 'label-settings+set-shadow-size)

(defgproperty label-settings+shadow-color 'label-settings :get
 'label-settings+get-shadow-color :set 'label-settings+set-shadow-color)

(defgproperty label-settings+shadow-offset 'label-settings :get
 'label-settings+get-shadow-offset :set 'label-settings+set-shadow-offset)

(defgproperty label-settings+stacked-outline-count 'label-settings :get
 'label-settings+get-stacked-outline-count :set
 'label-settings+set-stacked-outline-count)

(defgproperty label-settings+stacked-shadow-count 'label-settings :get
 'label-settings+get-stacked-shadow-count :set
 'label-settings+set-stacked-shadow-count)