(common-lisp:in-package :%godot)


(defgmethod
 (upnp+get-device-count :class 'upnp :bind "get_device_count" :hash 3905245786)
 int)

(defgmethod (upnp+get-device :class 'upnp :bind "get_device" :hash 2193290270)
 upnpdevice (index int))

(defgmethod (upnp+add-device :class 'upnp :bind "add_device" :hash 986715920)
 :void (device upnpdevice))

(defgmethod (upnp+set-device :class 'upnp :bind "set_device" :hash 3015133723)
 :void (index int) (device upnpdevice))

(defgmethod
 (upnp+remove-device :class 'upnp :bind "remove_device" :hash 1286410249) :void
 (index int))

(defgmethod
 (upnp+clear-devices :class 'upnp :bind "clear_devices" :hash 3218959716) :void)

(defgmethod
 (upnp+get-gateway :class 'upnp :bind "get_gateway" :hash 2276800779)
 upnpdevice)

(defgmethod (upnp+discover :class 'upnp :bind "discover" :hash 1575334765) int
 (timeout int) (ttl int) (device-filter string))

(defgmethod
 (upnp+query-external-address :class 'upnp :bind "query_external_address" :hash
  201670096)
 string)

(defgmethod
 (upnp+add-port-mapping :class 'upnp :bind "add_port_mapping" :hash 818314583)
 int (port int) (port-internal int) (desc string) (proto string) (duration int))

(defgmethod
 (upnp+delete-port-mapping :class 'upnp :bind "delete_port_mapping" :hash
  3444187325)
 int (port int) (proto string))

(defgmethod
 (upnp+set-discover-multicast-if :class 'upnp :bind "set_discover_multicast_if"
  :hash 83702148)
 :void (m-if string))

(defgmethod
 (upnp+get-discover-multicast-if :class 'upnp :bind "get_discover_multicast_if"
  :hash 201670096)
 string)

(defgmethod
 (upnp+set-discover-local-port :class 'upnp :bind "set_discover_local_port"
  :hash 1286410249)
 :void (port int))

(defgmethod
 (upnp+get-discover-local-port :class 'upnp :bind "get_discover_local_port"
  :hash 3905245786)
 int)

(defgmethod
 (upnp+set-discover-ipv6 :class 'upnp :bind "set_discover_ipv6" :hash
  2586408642)
 :void (ipv6 bool))

(defgmethod
 (upnp+is-discover-ipv6 :class 'upnp :bind "is_discover_ipv6" :hash 36873697)
 bool)