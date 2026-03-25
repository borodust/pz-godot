(common-lisp:in-package :%godot)


(defgmethod
 (input-map+has-action :class 'input-map :bind "has_action" :hash 2619796661)
 bool (action string-name))

(defgmethod
 (input-map+get-actions :class 'input-map :bind "get_actions" :hash 2915620761)
 array)

(defgmethod
 (input-map+add-action :class 'input-map :bind "add_action" :hash 1195233573)
 :void (action string-name) (deadzone float))

(defgmethod
 (input-map+erase-action :class 'input-map :bind "erase_action" :hash
  3304788590)
 :void (action string-name))

(defgmethod
 (input-map+get-action-description :class 'input-map :bind
  "get_action_description" :hash 957595536)
 string (action string-name))

(defgmethod
 (input-map+action-set-deadzone :class 'input-map :bind "action_set_deadzone"
  :hash 4135858297)
 :void (action string-name) (deadzone float))

(defgmethod
 (input-map+action-get-deadzone :class 'input-map :bind "action_get_deadzone"
  :hash 1391627649)
 float (action string-name))

(defgmethod
 (input-map+action-add-event :class 'input-map :bind "action_add_event" :hash
  518302593)
 :void (action string-name) (event input-event))

(defgmethod
 (input-map+action-has-event :class 'input-map :bind "action_has_event" :hash
  1185871985)
 bool (action string-name) (event input-event))

(defgmethod
 (input-map+action-erase-event :class 'input-map :bind "action_erase_event"
  :hash 518302593)
 :void (action string-name) (event input-event))

(defgmethod
 (input-map+action-erase-events :class 'input-map :bind "action_erase_events"
  :hash 3304788590)
 :void (action string-name))

(defgmethod
 (input-map+action-get-events :class 'input-map :bind "action_get_events" :hash
  689397652)
 array (action string-name))

(defgmethod
 (input-map+event-is-action :class 'input-map :bind "event_is_action" :hash
  3193353650)
 bool (event input-event) (action string-name) (exact-match bool))

(defgmethod
 (input-map+load-from-project-settings :class 'input-map :bind
  "load_from_project_settings" :hash 3218959716)
 :void)