(common-lisp:in-package :%godot)


(defgmethod
 (input-event-shortcut+set-shortcut :class 'input-event-shortcut :bind
  "set_shortcut" :hash 857163497)
 :void (shortcut shortcut))

(defgmethod
 (input-event-shortcut+get-shortcut :class 'input-event-shortcut :bind
  "get_shortcut" :hash 3766804753)
 shortcut)