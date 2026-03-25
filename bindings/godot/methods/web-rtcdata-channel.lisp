(common-lisp:in-package :%godot)


(defgmethod
 (web-rtcdata-channel+poll :class 'web-rtcdata-channel :bind "poll" :hash
  166280745)
 error)

(defgmethod
 (web-rtcdata-channel+close :class 'web-rtcdata-channel :bind "close" :hash
  3218959716)
 :void)

(defgmethod
 (web-rtcdata-channel+was-string-packet :class 'web-rtcdata-channel :bind
  "was_string_packet" :hash 36873697)
 bool)

(defgmethod
 (web-rtcdata-channel+set-write-mode :class 'web-rtcdata-channel :bind
  "set_write_mode" :hash 1999768052)
 :void (write-mode web-rtcdata-channel+write-mode))

(defgmethod
 (web-rtcdata-channel+get-write-mode :class 'web-rtcdata-channel :bind
  "get_write_mode" :hash 2848495172)
 web-rtcdata-channel+write-mode)

(defgmethod
 (web-rtcdata-channel+get-ready-state :class 'web-rtcdata-channel :bind
  "get_ready_state" :hash 3501143017)
 web-rtcdata-channel+channel-state)

(defgmethod
 (web-rtcdata-channel+get-label :class 'web-rtcdata-channel :bind "get_label"
  :hash 201670096)
 string)

(defgmethod
 (web-rtcdata-channel+is-ordered :class 'web-rtcdata-channel :bind "is_ordered"
  :hash 36873697)
 bool)

(defgmethod
 (web-rtcdata-channel+get-id :class 'web-rtcdata-channel :bind "get_id" :hash
  3905245786)
 int)

(defgmethod
 (web-rtcdata-channel+get-max-packet-life-time :class 'web-rtcdata-channel
  :bind "get_max_packet_life_time" :hash 3905245786)
 int)

(defgmethod
 (web-rtcdata-channel+get-max-retransmits :class 'web-rtcdata-channel :bind
  "get_max_retransmits" :hash 3905245786)
 int)

(defgmethod
 (web-rtcdata-channel+get-protocol :class 'web-rtcdata-channel :bind
  "get_protocol" :hash 201670096)
 string)

(defgmethod
 (web-rtcdata-channel+is-negotiated :class 'web-rtcdata-channel :bind
  "is_negotiated" :hash 36873697)
 bool)

(defgmethod
 (web-rtcdata-channel+get-buffered-amount :class 'web-rtcdata-channel :bind
  "get_buffered_amount" :hash 3905245786)
 int)