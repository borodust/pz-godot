(common-lisp:in-package :%godot)


(defgproperty input-event-midi+channel 'input-event-midi :get
 'input-event-midi+get-channel :set 'input-event-midi+set-channel)

(defgproperty input-event-midi+message 'input-event-midi :get
 'input-event-midi+get-message :set 'input-event-midi+set-message)

(defgproperty input-event-midi+pitch 'input-event-midi :get
 'input-event-midi+get-pitch :set 'input-event-midi+set-pitch)

(defgproperty input-event-midi+velocity 'input-event-midi :get
 'input-event-midi+get-velocity :set 'input-event-midi+set-velocity)

(defgproperty input-event-midi+instrument 'input-event-midi :get
 'input-event-midi+get-instrument :set 'input-event-midi+set-instrument)

(defgproperty input-event-midi+pressure 'input-event-midi :get
 'input-event-midi+get-pressure :set 'input-event-midi+set-pressure)

(defgproperty input-event-midi+controller-number 'input-event-midi :get
 'input-event-midi+get-controller-number :set
 'input-event-midi+set-controller-number)

(defgproperty input-event-midi+controller-value 'input-event-midi :get
 'input-event-midi+get-controller-value :set
 'input-event-midi+set-controller-value)