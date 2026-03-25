(common-lisp:in-package :%godot)


(defgproperty upnpdevice+description-url 'upnpdevice :get
 'upnpdevice+get-description-url :set 'upnpdevice+set-description-url)

(defgproperty upnpdevice+service-type 'upnpdevice :get
 'upnpdevice+get-service-type :set 'upnpdevice+set-service-type)

(defgproperty upnpdevice+igd-control-url 'upnpdevice :get
 'upnpdevice+get-igd-control-url :set 'upnpdevice+set-igd-control-url)

(defgproperty upnpdevice+igd-service-type 'upnpdevice :get
 'upnpdevice+get-igd-service-type :set 'upnpdevice+set-igd-service-type)

(defgproperty upnpdevice+igd-our-addr 'upnpdevice :get
 'upnpdevice+get-igd-our-addr :set 'upnpdevice+set-igd-our-addr)

(defgproperty upnpdevice+igd-status 'upnpdevice :get 'upnpdevice+get-igd-status
 :set 'upnpdevice+set-igd-status)