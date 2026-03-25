(common-lisp:in-package :%godot)


(defgmethod
 (web-rtcpeer-connection+set-default-extension :class 'web-rtcpeer-connection
  :bind "set_default_extension" :hash 3304788590 :static common-lisp:t)
 :void (extension-class string-name))

(defgmethod
 (web-rtcpeer-connection+initialize :class 'web-rtcpeer-connection :bind
  "initialize" :hash 2625064318)
 error (configuration dictionary))

(defgmethod
 (web-rtcpeer-connection+create-data-channel :class 'web-rtcpeer-connection
  :bind "create_data_channel" :hash 1288557393)
 web-rtcdata-channel (label string) (options dictionary))

(defgmethod
 (web-rtcpeer-connection+create-offer :class 'web-rtcpeer-connection :bind
  "create_offer" :hash 166280745)
 error)

(defgmethod
 (web-rtcpeer-connection+set-local-description :class 'web-rtcpeer-connection
  :bind "set_local_description" :hash 852856452)
 error (type string) (sdp string))

(defgmethod
 (web-rtcpeer-connection+set-remote-description :class 'web-rtcpeer-connection
  :bind "set_remote_description" :hash 852856452)
 error (type string) (sdp string))

(defgmethod
 (web-rtcpeer-connection+add-ice-candidate :class 'web-rtcpeer-connection :bind
  "add_ice_candidate" :hash 3958950400)
 error (media string) (index int) (name string))

(defgmethod
 (web-rtcpeer-connection+poll :class 'web-rtcpeer-connection :bind "poll" :hash
  166280745)
 error)

(defgmethod
 (web-rtcpeer-connection+close :class 'web-rtcpeer-connection :bind "close"
  :hash 3218959716)
 :void)

(defgmethod
 (web-rtcpeer-connection+get-connection-state :class 'web-rtcpeer-connection
  :bind "get_connection_state" :hash 2275710506)
 web-rtcpeer-connection+connection-state)

(defgmethod
 (web-rtcpeer-connection+get-gathering-state :class 'web-rtcpeer-connection
  :bind "get_gathering_state" :hash 4262591401)
 web-rtcpeer-connection+gathering-state)

(defgmethod
 (web-rtcpeer-connection+get-signaling-state :class 'web-rtcpeer-connection
  :bind "get_signaling_state" :hash 3342956226)
 web-rtcpeer-connection+signaling-state)