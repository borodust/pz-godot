(common-lisp:in-package :%godot)


(defgproperty xrinterface+interface-is-primary 'xrinterface :get
 'xrinterface+is-primary :set 'xrinterface+set-primary)

(defgproperty xrinterface+xr-play-area-mode 'xrinterface :get
 'xrinterface+get-play-area-mode :set 'xrinterface+set-play-area-mode)

(defgproperty xrinterface+environment-blend-mode 'xrinterface :get
 'xrinterface+get-environment-blend-mode :set
 'xrinterface+set-environment-blend-mode)

(defgproperty xrinterface+ar-is-anchor-detection-enabled 'xrinterface :get
 'xrinterface+get-anchor-detection-is-enabled :set
 'xrinterface+set-anchor-detection-is-enabled)