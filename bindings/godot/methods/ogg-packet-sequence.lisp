(common-lisp:in-package :%godot)


(defgmethod
 (ogg-packet-sequence+set-packet-data :class 'ogg-packet-sequence :bind
  "set_packet_data" :hash 381264803)
 :void (packet-data array))

(defgmethod
 (ogg-packet-sequence+get-packet-data :class 'ogg-packet-sequence :bind
  "get_packet_data" :hash 3995934104)
 array)

(defgmethod
 (ogg-packet-sequence+set-packet-granule-positions :class 'ogg-packet-sequence
  :bind "set_packet_granule_positions" :hash 3709968205)
 :void (granule-positions packed-int-64array))

(defgmethod
 (ogg-packet-sequence+get-packet-granule-positions :class 'ogg-packet-sequence
  :bind "get_packet_granule_positions" :hash 235988956)
 packed-int-64array)

(defgmethod
 (ogg-packet-sequence+set-sampling-rate :class 'ogg-packet-sequence :bind
  "set_sampling_rate" :hash 373806689)
 :void (sampling-rate float))

(defgmethod
 (ogg-packet-sequence+get-sampling-rate :class 'ogg-packet-sequence :bind
  "get_sampling_rate" :hash 1740695150)
 float)

(defgmethod
 (ogg-packet-sequence+get-length :class 'ogg-packet-sequence :bind "get_length"
  :hash 1740695150)
 float)