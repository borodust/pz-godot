(common-lisp:in-package :%godot)


(defgmethod
 (upnpdevice+is-valid-gateway :class 'upnpdevice :bind "is_valid_gateway" :hash
  36873697)
 bool)

(defgmethod
 (upnpdevice+query-external-address :class 'upnpdevice :bind
  "query_external_address" :hash 201670096)
 string)

(defgmethod
 (upnpdevice+add-port-mapping :class 'upnpdevice :bind "add_port_mapping" :hash
  818314583)
 int (port int) (port-internal int) (desc string) (proto string) (duration int))

(defgmethod
 (upnpdevice+delete-port-mapping :class 'upnpdevice :bind "delete_port_mapping"
  :hash 3444187325)
 int (port int) (proto string))

(defgmethod
 (upnpdevice+set-description-url :class 'upnpdevice :bind "set_description_url"
  :hash 83702148)
 :void (url string))

(defgmethod
 (upnpdevice+get-description-url :class 'upnpdevice :bind "get_description_url"
  :hash 201670096)
 string)

(defgmethod
 (upnpdevice+set-service-type :class 'upnpdevice :bind "set_service_type" :hash
  83702148)
 :void (type string))

(defgmethod
 (upnpdevice+get-service-type :class 'upnpdevice :bind "get_service_type" :hash
  201670096)
 string)

(defgmethod
 (upnpdevice+set-igd-control-url :class 'upnpdevice :bind "set_igd_control_url"
  :hash 83702148)
 :void (url string))

(defgmethod
 (upnpdevice+get-igd-control-url :class 'upnpdevice :bind "get_igd_control_url"
  :hash 201670096)
 string)

(defgmethod
 (upnpdevice+set-igd-service-type :class 'upnpdevice :bind
  "set_igd_service_type" :hash 83702148)
 :void (type string))

(defgmethod
 (upnpdevice+get-igd-service-type :class 'upnpdevice :bind
  "get_igd_service_type" :hash 201670096)
 string)

(defgmethod
 (upnpdevice+set-igd-our-addr :class 'upnpdevice :bind "set_igd_our_addr" :hash
  83702148)
 :void (addr string))

(defgmethod
 (upnpdevice+get-igd-our-addr :class 'upnpdevice :bind "get_igd_our_addr" :hash
  201670096)
 string)

(defgmethod
 (upnpdevice+set-igd-status :class 'upnpdevice :bind "set_igd_status" :hash
  519504122)
 :void (status upnpdevice+igdstatus))

(defgmethod
 (upnpdevice+get-igd-status :class 'upnpdevice :bind "get_igd_status" :hash
  180887011)
 upnpdevice+igdstatus)