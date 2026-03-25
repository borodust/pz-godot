(common-lisp:in-package :%godot)


(defgproperty xrtracker+type 'xrtracker :get 'xrtracker+get-tracker-type :set
 'xrtracker+set-tracker-type)

(defgproperty xrtracker+name 'xrtracker :get 'xrtracker+get-tracker-name :set
 'xrtracker+set-tracker-name)

(defgproperty xrtracker+description 'xrtracker :get 'xrtracker+get-tracker-desc
 :set 'xrtracker+set-tracker-desc)