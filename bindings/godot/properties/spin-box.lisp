(common-lisp:in-package :%godot)


(defgproperty spin-box+alignment 'spin-box :get
 'spin-box+get-horizontal-alignment :set 'spin-box+set-horizontal-alignment)

(defgproperty spin-box+editable 'spin-box :get 'spin-box+is-editable :set
 'spin-box+set-editable)

(defgproperty spin-box+update-on-text-changed 'spin-box :get
 'spin-box+get-update-on-text-changed :set 'spin-box+set-update-on-text-changed)

(defgproperty spin-box+prefix 'spin-box :get 'spin-box+get-prefix :set
 'spin-box+set-prefix)

(defgproperty spin-box+suffix 'spin-box :get 'spin-box+get-suffix :set
 'spin-box+set-suffix)

(defgproperty spin-box+custom-arrow-step 'spin-box :get
 'spin-box+get-custom-arrow-step :set 'spin-box+set-custom-arrow-step)

(defgproperty spin-box+custom-arrow-round 'spin-box :get
 'spin-box+is-custom-arrow-rounding :set 'spin-box+set-custom-arrow-round)

(defgproperty spin-box+select-all-on-focus 'spin-box :get
 'spin-box+is-select-all-on-focus :set 'spin-box+set-select-all-on-focus)