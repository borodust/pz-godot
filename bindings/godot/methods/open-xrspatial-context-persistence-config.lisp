(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-context-persistence-config+add-persistence-context :class
  'open-xrspatial-context-persistence-config :bind "add_persistence_context"
  :hash 2722037293)
 :void (persistence-context rid))

(defgmethod
 (open-xrspatial-context-persistence-config+remove-persistence-context :class
  'open-xrspatial-context-persistence-config :bind "remove_persistence_context"
  :hash 2722037293)
 :void (persistence-context rid))