(common-lisp:in-package :%godot)


(defgmethod
 (open-xrandroid-thread-settings-extension+set-application-thread-type :class
  'open-xrandroid-thread-settings-extension :bind "set_application_thread_type"
  :hash 1558751158)
 bool (thread-type open-xrandroid-thread-settings-extension+thread-type)
 (thread-id int))