(common-lisp:in-package :%godot)


(defgproperty random-number-generator+seed 'random-number-generator :get
 'random-number-generator+get-seed :set 'random-number-generator+set-seed)

(defgproperty random-number-generator+state 'random-number-generator :get
 'random-number-generator+get-state :set 'random-number-generator+set-state)