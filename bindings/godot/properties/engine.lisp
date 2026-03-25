(common-lisp:in-package :%godot)


(defgproperty engine+print-error-messages 'engine :get
 'engine+is-printing-error-messages :set 'engine+set-print-error-messages)

(defgproperty engine+print-to-stdout 'engine :get 'engine+is-printing-to-stdout
 :set 'engine+set-print-to-stdout)

(defgproperty engine+physics-ticks-per-second 'engine :get
 'engine+get-physics-ticks-per-second :set 'engine+set-physics-ticks-per-second)

(defgproperty engine+max-physics-steps-per-frame 'engine :get
 'engine+get-max-physics-steps-per-frame :set
 'engine+set-max-physics-steps-per-frame)

(defgproperty engine+max-fps 'engine :get 'engine+get-max-fps :set
 'engine+set-max-fps)

(defgproperty engine+time-scale 'engine :get 'engine+get-time-scale :set
 'engine+set-time-scale)

(defgproperty engine+physics-jitter-fix 'engine :get
 'engine+get-physics-jitter-fix :set 'engine+set-physics-jitter-fix)