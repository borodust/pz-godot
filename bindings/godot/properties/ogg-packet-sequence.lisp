(common-lisp:in-package :%godot)


(defgproperty ogg-packet-sequence+packet-data 'ogg-packet-sequence :get
 'ogg-packet-sequence+get-packet-data :set 'ogg-packet-sequence+set-packet-data)

(defgproperty ogg-packet-sequence+granule-positions 'ogg-packet-sequence :get
 'ogg-packet-sequence+get-packet-granule-positions :set
 'ogg-packet-sequence+set-packet-granule-positions)

(defgproperty ogg-packet-sequence+sampling-rate 'ogg-packet-sequence :get
 'ogg-packet-sequence+get-sampling-rate :set
 'ogg-packet-sequence+set-sampling-rate)