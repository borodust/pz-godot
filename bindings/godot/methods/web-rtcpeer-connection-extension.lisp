(common-lisp:in-package :%godot)


(defgmethod
 (web-rtcpeer-connection-extension+-get-connection-state :class
  'web-rtcpeer-connection-extension :bind "_get_connection_state" :hash
  2275710506 :virtual common-lisp:t)
 web-rtcpeer-connection+connection-state)

(defgmethod
 (web-rtcpeer-connection-extension+-get-gathering-state :class
  'web-rtcpeer-connection-extension :bind "_get_gathering_state" :hash
  4262591401 :virtual common-lisp:t)
 web-rtcpeer-connection+gathering-state)

(defgmethod
 (web-rtcpeer-connection-extension+-get-signaling-state :class
  'web-rtcpeer-connection-extension :bind "_get_signaling_state" :hash
  3342956226 :virtual common-lisp:t)
 web-rtcpeer-connection+signaling-state)

(defgmethod
 (web-rtcpeer-connection-extension+-initialize :class
  'web-rtcpeer-connection-extension :bind "_initialize" :hash 1494659981
  :virtual common-lisp:t)
 error (p-config dictionary))

(defgmethod
 (web-rtcpeer-connection-extension+-create-data-channel :class
  'web-rtcpeer-connection-extension :bind "_create_data_channel" :hash
  4111292546 :virtual common-lisp:t)
 web-rtcdata-channel (p-label string) (p-config dictionary))

(defgmethod
 (web-rtcpeer-connection-extension+-create-offer :class
  'web-rtcpeer-connection-extension :bind "_create_offer" :hash 166280745
  :virtual common-lisp:t)
 error)

(defgmethod
 (web-rtcpeer-connection-extension+-set-remote-description :class
  'web-rtcpeer-connection-extension :bind "_set_remote_description" :hash
  852856452 :virtual common-lisp:t)
 error (p-type string) (p-sdp string))

(defgmethod
 (web-rtcpeer-connection-extension+-set-local-description :class
  'web-rtcpeer-connection-extension :bind "_set_local_description" :hash
  852856452 :virtual common-lisp:t)
 error (p-type string) (p-sdp string))

(defgmethod
 (web-rtcpeer-connection-extension+-add-ice-candidate :class
  'web-rtcpeer-connection-extension :bind "_add_ice_candidate" :hash 3958950400
  :virtual common-lisp:t)
 error (p-sdp-mid-name string) (p-sdp-mline-index int) (p-sdp-name string))

(defgmethod
 (web-rtcpeer-connection-extension+-poll :class
  'web-rtcpeer-connection-extension :bind "_poll" :hash 166280745 :virtual
  common-lisp:t)
 error)

(defgmethod
 (web-rtcpeer-connection-extension+-close :class
  'web-rtcpeer-connection-extension :bind "_close" :hash 3218959716 :virtual
  common-lisp:t)
 :void)