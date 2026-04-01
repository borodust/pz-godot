(common-lisp:in-package :%godot)


(defgmethod
 (web-rtcdata-channel-extension+%get-packet :class
  'web-rtcdata-channel-extension :bind "_get_packet" :hash 3099858825 :virtual
  common-lisp:t)
 error (r-buffer (:pointer (:pointer :uint8)))
 (r-buffer-size (:pointer :int32)))

(defgmethod
 (web-rtcdata-channel-extension+%put-packet :class
  'web-rtcdata-channel-extension :bind "_put_packet" :hash 3099858825 :virtual
  common-lisp:t)
 error (p-buffer (:pointer :uint8)) (p-buffer-size int))

(defgmethod
 (web-rtcdata-channel-extension+%get-available-packet-count :class
  'web-rtcdata-channel-extension :bind "_get_available_packet_count" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (web-rtcdata-channel-extension+%get-max-packet-size :class
  'web-rtcdata-channel-extension :bind "_get_max_packet_size" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (web-rtcdata-channel-extension+%poll :class 'web-rtcdata-channel-extension
  :bind "_poll" :hash 166280745 :virtual common-lisp:t)
 error)

(defgmethod
 (web-rtcdata-channel-extension+%close :class 'web-rtcdata-channel-extension
  :bind "_close" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (web-rtcdata-channel-extension+%set-write-mode :class
  'web-rtcdata-channel-extension :bind "_set_write_mode" :hash 1999768052
  :virtual common-lisp:t)
 :void (p-write-mode web-rtcdata-channel+write-mode))

(defgmethod
 (web-rtcdata-channel-extension+%get-write-mode :class
  'web-rtcdata-channel-extension :bind "_get_write_mode" :hash 2848495172
  :virtual common-lisp:t)
 web-rtcdata-channel+write-mode)

(defgmethod
 (web-rtcdata-channel-extension+%was-string-packet :class
  'web-rtcdata-channel-extension :bind "_was_string_packet" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (web-rtcdata-channel-extension+%get-ready-state :class
  'web-rtcdata-channel-extension :bind "_get_ready_state" :hash 3501143017
  :virtual common-lisp:t)
 web-rtcdata-channel+channel-state)

(defgmethod
 (web-rtcdata-channel-extension+%get-label :class
  'web-rtcdata-channel-extension :bind "_get_label" :hash 201670096 :virtual
  common-lisp:t)
 string)

(defgmethod
 (web-rtcdata-channel-extension+%is-ordered :class
  'web-rtcdata-channel-extension :bind "_is_ordered" :hash 36873697 :virtual
  common-lisp:t)
 bool)

(defgmethod
 (web-rtcdata-channel-extension+%get-id :class 'web-rtcdata-channel-extension
  :bind "_get_id" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (web-rtcdata-channel-extension+%get-max-packet-life-time :class
  'web-rtcdata-channel-extension :bind "_get_max_packet_life_time" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (web-rtcdata-channel-extension+%get-max-retransmits :class
  'web-rtcdata-channel-extension :bind "_get_max_retransmits" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (web-rtcdata-channel-extension+%get-protocol :class
  'web-rtcdata-channel-extension :bind "_get_protocol" :hash 201670096 :virtual
  common-lisp:t)
 string)

(defgmethod
 (web-rtcdata-channel-extension+%is-negotiated :class
  'web-rtcdata-channel-extension :bind "_is_negotiated" :hash 36873697 :virtual
  common-lisp:t)
 bool)

(defgmethod
 (web-rtcdata-channel-extension+%get-buffered-amount :class
  'web-rtcdata-channel-extension :bind "_get_buffered_amount" :hash 3905245786
  :virtual common-lisp:t)
 int)