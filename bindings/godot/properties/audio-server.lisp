(common-lisp:in-package :%godot)


(defgproperty audio-server+bus-count 'audio-server :get
 'audio-server+get-bus-count :set 'audio-server+set-bus-count)

(defgproperty audio-server+output-device 'audio-server :get
 'audio-server+get-output-device :set 'audio-server+set-output-device)

(defgproperty audio-server+input-device 'audio-server :get
 'audio-server+get-input-device :set 'audio-server+set-input-device)

(defgproperty audio-server+playback-speed-scale 'audio-server :get
 'audio-server+get-playback-speed-scale :set
 'audio-server+set-playback-speed-scale)