(common-lisp:in-package :%godot)


(defgproperty web-xrinterface+session-mode 'web-xrinterface :get
 'web-xrinterface+get-session-mode :set 'web-xrinterface+set-session-mode)

(defgproperty web-xrinterface+required-features 'web-xrinterface :get
 'web-xrinterface+get-required-features :set
 'web-xrinterface+set-required-features)

(defgproperty web-xrinterface+optional-features 'web-xrinterface :get
 'web-xrinterface+get-optional-features :set
 'web-xrinterface+set-optional-features)

(defgproperty web-xrinterface+requested-reference-space-types 'web-xrinterface
 :get 'web-xrinterface+get-requested-reference-space-types :set
 'web-xrinterface+set-requested-reference-space-types)

(defgproperty web-xrinterface+reference-space-type 'web-xrinterface :get
 'web-xrinterface+get-reference-space-type)

(defgproperty web-xrinterface+enabled-features 'web-xrinterface :get
 'web-xrinterface+get-enabled-features)

(defgproperty web-xrinterface+visibility-state 'web-xrinterface :get
 'web-xrinterface+get-visibility-state)