(common-lisp:in-package :%godot)


(defgmethod
 (ip+resolve-hostname :class 'ip :bind "resolve_hostname" :hash 4283295457)
 string (host string) (ip-type ip+type))

(defgmethod
 (ip+resolve-hostname-addresses :class 'ip :bind "resolve_hostname_addresses"
  :hash 773767525)
 packed-string-array (host string) (ip-type ip+type))

(defgmethod
 (ip+resolve-hostname-queue-item :class 'ip :bind "resolve_hostname_queue_item"
  :hash 1749894742)
 int (host string) (ip-type ip+type))

(defgmethod
 (ip+get-resolve-item-status :class 'ip :bind "get_resolve_item_status" :hash
  3812250196)
 ip+resolver-status (id int))

(defgmethod
 (ip+get-resolve-item-address :class 'ip :bind "get_resolve_item_address" :hash
  844755477)
 string (id int))

(defgmethod
 (ip+get-resolve-item-addresses :class 'ip :bind "get_resolve_item_addresses"
  :hash 663333327)
 array (id int))

(defgmethod
 (ip+erase-resolve-item :class 'ip :bind "erase_resolve_item" :hash 1286410249)
 :void (id int))

(defgmethod
 (ip+get-local-addresses :class 'ip :bind "get_local_addresses" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (ip+get-local-interfaces :class 'ip :bind "get_local_interfaces" :hash
  3995934104)
 array)

(defgmethod (ip+clear-cache :class 'ip :bind "clear_cache" :hash 3005725572)
 :void (hostname string))