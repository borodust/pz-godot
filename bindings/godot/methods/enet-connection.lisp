(common-lisp:in-package :%godot)


(defgmethod
 (enet-connection+create-host-bound :class 'enet-connection :bind
  "create_host_bound" :hash 1515002313)
 error (bind-address string) (bind-port int) (max-peers int) (max-channels int)
 (in-bandwidth int) (out-bandwidth int))

(defgmethod
 (enet-connection+create-host :class 'enet-connection :bind "create_host" :hash
  117198950)
 error (max-peers int) (max-channels int) (in-bandwidth int)
 (out-bandwidth int))

(defgmethod
 (enet-connection+destroy :class 'enet-connection :bind "destroy" :hash
  3218959716)
 :void)

(defgmethod
 (enet-connection+connect-to-host :class 'enet-connection :bind
  "connect_to_host" :hash 2171300490)
 enet-packet-peer (address string) (port int) (channels int) (data int))

(defgmethod
 (enet-connection+service :class 'enet-connection :bind "service" :hash
  2402345344)
 array (timeout int))

(defgmethod
 (enet-connection+flush :class 'enet-connection :bind "flush" :hash 3218959716)
 :void)

(defgmethod
 (enet-connection+bandwidth-limit :class 'enet-connection :bind
  "bandwidth_limit" :hash 2302169788)
 :void (in-bandwidth int) (out-bandwidth int))

(defgmethod
 (enet-connection+channel-limit :class 'enet-connection :bind "channel_limit"
  :hash 1286410249)
 :void (limit int))

(defgmethod
 (enet-connection+broadcast :class 'enet-connection :bind "broadcast" :hash
  2772371345)
 :void (channel int) (packet packed-byte-array) (flags int))

(defgmethod
 (enet-connection+compress :class 'enet-connection :bind "compress" :hash
  2660215187)
 :void (mode enet-connection+compression-mode))

(defgmethod
 (enet-connection+dtls-server-setup :class 'enet-connection :bind
  "dtls_server_setup" :hash 1262296096)
 error (server-options tlsoptions))

(defgmethod
 (enet-connection+dtls-client-setup :class 'enet-connection :bind
  "dtls_client_setup" :hash 1966198364)
 error (hostname string) (client-options tlsoptions))

(defgmethod
 (enet-connection+refuse-new-connections :class 'enet-connection :bind
  "refuse_new_connections" :hash 2586408642)
 :void (refuse bool))

(defgmethod
 (enet-connection+pop-statistic :class 'enet-connection :bind "pop_statistic"
  :hash 2166904170)
 float (statistic enet-connection+host-statistic))

(defgmethod
 (enet-connection+get-max-channels :class 'enet-connection :bind
  "get_max_channels" :hash 3905245786)
 int)

(defgmethod
 (enet-connection+get-local-port :class 'enet-connection :bind "get_local_port"
  :hash 3905245786)
 int)

(defgmethod
 (enet-connection+get-peers :class 'enet-connection :bind "get_peers" :hash
  2915620761)
 array)

(defgmethod
 (enet-connection+socket-send :class 'enet-connection :bind "socket_send" :hash
  1100646812)
 :void (destination-address string) (destination-port int)
 (packet packed-byte-array))