(common-lisp:in-package :%godot)


(defgproperty timer+process-callback 'timer :get
 'timer+get-timer-process-callback :set 'timer+set-timer-process-callback)

(defgproperty timer+wait-time 'timer :get 'timer+get-wait-time :set
 'timer+set-wait-time)

(defgproperty timer+one-shot 'timer :get 'timer+is-one-shot :set
 'timer+set-one-shot)

(defgproperty timer+autostart 'timer :get 'timer+has-autostart :set
 'timer+set-autostart)

(defgproperty timer+paused 'timer :get 'timer+is-paused :set 'timer+set-paused)

(defgproperty timer+ignore-time-scale 'timer :get 'timer+is-ignoring-time-scale
 :set 'timer+set-ignore-time-scale)

(defgproperty timer+time-left 'timer :get 'timer+get-time-left)