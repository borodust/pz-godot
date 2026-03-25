(common-lisp:in-package :%godot)


(defgproperty upnp+discover-multicast-if 'upnp :get
 'upnp+get-discover-multicast-if :set 'upnp+set-discover-multicast-if)

(defgproperty upnp+discover-local-port 'upnp :get 'upnp+get-discover-local-port
 :set 'upnp+set-discover-local-port)

(defgproperty upnp+discover-ipv6 'upnp :get 'upnp+is-discover-ipv6 :set
 'upnp+set-discover-ipv6)