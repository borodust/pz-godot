(common-lisp:in-package :%godot)


(defgproperty open-xrinterface+display-refresh-rate 'open-xrinterface :get
 'open-xrinterface+get-display-refresh-rate :set
 'open-xrinterface+set-display-refresh-rate)

(defgproperty open-xrinterface+render-target-size-multiplier 'open-xrinterface
 :get 'open-xrinterface+get-render-target-size-multiplier :set
 'open-xrinterface+set-render-target-size-multiplier)

(defgproperty open-xrinterface+foveation-level 'open-xrinterface :get
 'open-xrinterface+get-foveation-level :set
 'open-xrinterface+set-foveation-level)

(defgproperty open-xrinterface+foveation-dynamic 'open-xrinterface :get
 'open-xrinterface+get-foveation-dynamic :set
 'open-xrinterface+set-foveation-dynamic)

(defgproperty open-xrinterface+vrs-min-radius 'open-xrinterface :get
 'open-xrinterface+get-vrs-min-radius :set 'open-xrinterface+set-vrs-min-radius)

(defgproperty open-xrinterface+vrs-strength 'open-xrinterface :get
 'open-xrinterface+get-vrs-strength :set 'open-xrinterface+set-vrs-strength)