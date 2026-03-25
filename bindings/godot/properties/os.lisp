(common-lisp:in-package :%godot)


(defgproperty os+low-processor-usage-mode 'os :get
 'os+is-in-low-processor-usage-mode :set 'os+set-low-processor-usage-mode)

(defgproperty os+low-processor-usage-mode-sleep-usec 'os :get
 'os+get-low-processor-usage-mode-sleep-usec :set
 'os+set-low-processor-usage-mode-sleep-usec)

(defgproperty os+delta-smoothing 'os :get 'os+is-delta-smoothing-enabled :set
 'os+set-delta-smoothing)