(common-lisp:in-package :%godot)


(defgmethod
 (open-xraction-set+set-localized-name :class 'open-xraction-set :bind
  "set_localized_name" :hash 83702148)
 :void (localized-name string))

(defgmethod
 (open-xraction-set+get-localized-name :class 'open-xraction-set :bind
  "get_localized_name" :hash 201670096)
 string)

(defgmethod
 (open-xraction-set+set-priority :class 'open-xraction-set :bind "set_priority"
  :hash 1286410249)
 :void (priority int))

(defgmethod
 (open-xraction-set+get-priority :class 'open-xraction-set :bind "get_priority"
  :hash 3905245786)
 int)

(defgmethod
 (open-xraction-set+get-action-count :class 'open-xraction-set :bind
  "get_action_count" :hash 3905245786)
 int)

(defgmethod
 (open-xraction-set+set-actions :class 'open-xraction-set :bind "set_actions"
  :hash 381264803)
 :void (actions array))

(defgmethod
 (open-xraction-set+get-actions :class 'open-xraction-set :bind "get_actions"
  :hash 3995934104)
 array)

(defgmethod
 (open-xraction-set+add-action :class 'open-xraction-set :bind "add_action"
  :hash 349361333)
 :void (action open-xraction))

(defgmethod
 (open-xraction-set+remove-action :class 'open-xraction-set :bind
  "remove_action" :hash 349361333)
 :void (action open-xraction))