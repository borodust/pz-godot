(common-lisp:in-package :%godot)


(defgproperty input-event-mouse-motion+tilt 'input-event-mouse-motion :get
 'input-event-mouse-motion+get-tilt :set 'input-event-mouse-motion+set-tilt)

(defgproperty input-event-mouse-motion+pressure 'input-event-mouse-motion :get
 'input-event-mouse-motion+get-pressure :set
 'input-event-mouse-motion+set-pressure)

(defgproperty input-event-mouse-motion+pen-inverted 'input-event-mouse-motion
 :get 'input-event-mouse-motion+get-pen-inverted :set
 'input-event-mouse-motion+set-pen-inverted)

(defgproperty input-event-mouse-motion+relative 'input-event-mouse-motion :get
 'input-event-mouse-motion+get-relative :set
 'input-event-mouse-motion+set-relative)

(defgproperty input-event-mouse-motion+screen-relative
 'input-event-mouse-motion :get 'input-event-mouse-motion+get-screen-relative
 :set 'input-event-mouse-motion+set-screen-relative)

(defgproperty input-event-mouse-motion+velocity 'input-event-mouse-motion :get
 'input-event-mouse-motion+get-velocity :set
 'input-event-mouse-motion+set-velocity)

(defgproperty input-event-mouse-motion+screen-velocity
 'input-event-mouse-motion :get 'input-event-mouse-motion+get-screen-velocity
 :set 'input-event-mouse-motion+set-screen-velocity)