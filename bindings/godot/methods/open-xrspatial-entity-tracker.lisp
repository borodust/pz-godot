(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-entity-tracker+set-entity :class
  'open-xrspatial-entity-tracker :bind "set_entity" :hash 2722037293)
 :void (entity rid))

(defgmethod
 (open-xrspatial-entity-tracker+get-entity :class
  'open-xrspatial-entity-tracker :bind "get_entity" :hash 2944877500)
 rid)

(defgmethod
 (open-xrspatial-entity-tracker+set-spatial-tracking-state :class
  'open-xrspatial-entity-tracker :bind "set_spatial_tracking_state" :hash
  2170234447)
 :void
 (spatial-tracking-state open-xrspatial-entity-tracker+entity-tracking-state))

(defgmethod
 (open-xrspatial-entity-tracker+get-spatial-tracking-state :class
  'open-xrspatial-entity-tracker :bind "get_spatial_tracking_state" :hash
  3351876560)
 open-xrspatial-entity-tracker+entity-tracking-state)