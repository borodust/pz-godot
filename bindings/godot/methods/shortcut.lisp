(common-lisp:in-package :%godot)


(defgmethod
 (shortcut+set-events :class 'shortcut :bind "set_events" :hash 381264803)
 :void (events array))

(defgmethod
 (shortcut+get-events :class 'shortcut :bind "get_events" :hash 3995934104)
 array)

(defgmethod
 (shortcut+has-valid-event :class 'shortcut :bind "has_valid_event" :hash
  36873697)
 bool)

(defgmethod
 (shortcut+matches-event :class 'shortcut :bind "matches_event" :hash
  3738334489)
 bool (event input-event))

(defgmethod
 (shortcut+get-as-text :class 'shortcut :bind "get_as_text" :hash 201670096)
 string)