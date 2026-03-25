(common-lisp:in-package :%godot)


(defgproperty xrpose+has-tracking-data 'xrpose :get
 'xrpose+get-has-tracking-data :set 'xrpose+set-has-tracking-data)

(defgproperty xrpose+name 'xrpose :get 'xrpose+get-name :set 'xrpose+set-name)

(defgproperty xrpose+transform 'xrpose :get 'xrpose+get-transform :set
 'xrpose+set-transform)

(defgproperty xrpose+linear-velocity 'xrpose :get 'xrpose+get-linear-velocity
 :set 'xrpose+set-linear-velocity)

(defgproperty xrpose+angular-velocity 'xrpose :get 'xrpose+get-angular-velocity
 :set 'xrpose+set-angular-velocity)

(defgproperty xrpose+tracking-confidence 'xrpose :get
 'xrpose+get-tracking-confidence :set 'xrpose+set-tracking-confidence)