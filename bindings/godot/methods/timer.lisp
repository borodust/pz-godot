(common-lisp:in-package :%godot)


(defgmethod
 (timer+set-wait-time :class 'timer :bind "set_wait_time" :hash 373806689)
 :void (time-sec float))

(defgmethod
 (timer+get-wait-time :class 'timer :bind "get_wait_time" :hash 1740695150)
 float)

(defgmethod
 (timer+set-one-shot :class 'timer :bind "set_one_shot" :hash 2586408642) :void
 (enable bool))

(defgmethod
 (timer+is-one-shot :class 'timer :bind "is_one_shot" :hash 36873697) bool)

(defgmethod
 (timer+set-autostart :class 'timer :bind "set_autostart" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (timer+has-autostart :class 'timer :bind "has_autostart" :hash 36873697) bool)

(defgmethod (timer+start :class 'timer :bind "start" :hash 1392008558) :void
 (time-sec float))

(defgmethod (timer+stop :class 'timer :bind "stop" :hash 3218959716) :void)

(defgmethod
 (timer+set-paused :class 'timer :bind "set_paused" :hash 2586408642) :void
 (paused bool))

(defgmethod (timer+is-paused :class 'timer :bind "is_paused" :hash 36873697)
 bool)

(defgmethod
 (timer+set-ignore-time-scale :class 'timer :bind "set_ignore_time_scale" :hash
  2586408642)
 :void (ignore bool))

(defgmethod
 (timer+is-ignoring-time-scale :class 'timer :bind "is_ignoring_time_scale"
  :hash 2240911060)
 bool)

(defgmethod (timer+is-stopped :class 'timer :bind "is_stopped" :hash 36873697)
 bool)

(defgmethod
 (timer+get-time-left :class 'timer :bind "get_time_left" :hash 1740695150)
 float)

(defgmethod
 (timer+set-timer-process-callback :class 'timer :bind
  "set_timer_process_callback" :hash 3469495063)
 :void (callback timer+timer-process-callback))

(defgmethod
 (timer+get-timer-process-callback :class 'timer :bind
  "get_timer_process_callback" :hash 2672570227)
 timer+timer-process-callback)