(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-plane-tracking-capability+is-supported :class
  'open-xrspatial-plane-tracking-capability :bind "is_supported" :hash
  2240911060)
 bool)

(defgmethod
 (open-xrspatial-plane-tracking-capability+start-entity-discovery :class
  'open-xrspatial-plane-tracking-capability :bind "start_entity_discovery"
  :hash 3452714169)
 open-xrfuture-result (spatial-context rid) (component-data array)
 (next-snapshot-create open-xrstructure-base)
 (next-snapshot-query open-xrstructure-base) (user-callback callable))