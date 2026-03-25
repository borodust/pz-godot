(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-query-result-data+get-capacity :class
  'open-xrspatial-query-result-data :bind "get_capacity" :hash 3905245786)
 int)

(defgmethod
 (open-xrspatial-query-result-data+get-entity-id :class
  'open-xrspatial-query-result-data :bind "get_entity_id" :hash 923996154)
 int (index int))

(defgmethod
 (open-xrspatial-query-result-data+get-entity-state :class
  'open-xrspatial-query-result-data :bind "get_entity_state" :hash 1411962015)
 open-xrspatial-entity-tracker+entity-tracking-state (index int))