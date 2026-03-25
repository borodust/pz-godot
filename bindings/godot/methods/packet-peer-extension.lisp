(common-lisp:in-package :%godot)


(defgmethod
 (packet-peer-extension+-get-packet :class 'packet-peer-extension :bind
  "_get_packet" :hash 3099858825 :virtual common-lisp:t)
 error (r-buffer (:pointer (:pointer :uint8)))
 (r-buffer-size (:pointer :int32)))

(defgmethod
 (packet-peer-extension+-put-packet :class 'packet-peer-extension :bind
  "_put_packet" :hash 3099858825 :virtual common-lisp:t)
 error (p-buffer (:pointer :uint8)) (p-buffer-size int))

(defgmethod
 (packet-peer-extension+-get-available-packet-count :class
  'packet-peer-extension :bind "_get_available_packet_count" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (packet-peer-extension+-get-max-packet-size :class 'packet-peer-extension
  :bind "_get_max_packet_size" :hash 3905245786 :virtual common-lisp:t)
 int)