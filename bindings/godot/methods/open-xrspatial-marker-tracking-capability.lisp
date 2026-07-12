(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-marker-tracking-capability+is-qrcode-supported :class
  'open-xrspatial-marker-tracking-capability :bind "is_qrcode_supported" :hash
  2240911060)
 bool)

(defgmethod
 (open-xrspatial-marker-tracking-capability+is-micro-qrcode-supported :class
  'open-xrspatial-marker-tracking-capability :bind "is_micro_qrcode_supported"
  :hash 2240911060)
 bool)

(defgmethod
 (open-xrspatial-marker-tracking-capability+is-aruco-supported :class
  'open-xrspatial-marker-tracking-capability :bind "is_aruco_supported" :hash
  2240911060)
 bool)

(defgmethod
 (open-xrspatial-marker-tracking-capability+is-april-tag-supported :class
  'open-xrspatial-marker-tracking-capability :bind "is_april_tag_supported"
  :hash 2240911060)
 bool)

(defgmethod
 (open-xrspatial-marker-tracking-capability+start-entity-discovery :class
  'open-xrspatial-marker-tracking-capability :bind "start_entity_discovery"
  :hash 3452714169)
 open-xrfuture-result (spatial-context rid) (component-data array)
 (next-snapshot-create open-xrstructure-base)
 (next-snapshot-query open-xrstructure-base) (user-callback callable))

(defgmethod
 (open-xrspatial-marker-tracking-capability+do-entity-update :class
  'open-xrspatial-marker-tracking-capability :bind "do_entity_update" :hash
  3138044275)
 :void (spatial-context rid) (component-data array)
 (next-snapshot-create open-xrstructure-base)
 (next-snapshot-query open-xrstructure-base))