(common-lisp:in-package :%godot)


(defgproperty range+min-value 'range :get 'range+get-min :set 'range+set-min)

(defgproperty range+max-value 'range :get 'range+get-max :set 'range+set-max)

(defgproperty range+step 'range :get 'range+get-step :set 'range+set-step)

(defgproperty range+page 'range :get 'range+get-page :set 'range+set-page)

(defgproperty range+value 'range :get 'range+get-value :set 'range+set-value)

(defgproperty range+ratio 'range :get 'range+get-as-ratio :set
 'range+set-as-ratio)

(defgproperty range+exp-edit 'range :get 'range+is-ratio-exp :set
 'range+set-exp-ratio)

(defgproperty range+rounded 'range :get 'range+is-using-rounded-values :set
 'range+set-use-rounded-values)

(defgproperty range+allow-greater 'range :get 'range+is-greater-allowed :set
 'range+set-allow-greater)

(defgproperty range+allow-lesser 'range :get 'range+is-lesser-allowed :set
 'range+set-allow-lesser)