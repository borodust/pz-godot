(common-lisp:in-package :%godot)


(defgmethod (thread+start :class 'thread :bind "start" :hash 1327203254) error
 (callable callable) (priority thread+priority))

(defgmethod (thread+get-id :class 'thread :bind "get_id" :hash 201670096)
 string)

(defgmethod
 (thread+is-started :class 'thread :bind "is_started" :hash 36873697) bool)

(defgmethod (thread+is-alive :class 'thread :bind "is_alive" :hash 36873697)
 bool)

(defgmethod
 (thread+wait-to-finish :class 'thread :bind "wait_to_finish" :hash 1460262497)
 variant)

(defgmethod
 (thread+set-thread-safety-checks-enabled :class 'thread :bind
  "set_thread_safety_checks_enabled" :hash 2586408642 :static common-lisp:t)
 :void (enabled bool))

(defgmethod
 (thread+is-main-thread :class 'thread :bind "is_main_thread" :hash 2240911060
  :static common-lisp:t)
 bool)