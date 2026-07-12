(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-entity-tracker+set-spatial-context :class
  'open-xrspatial-entity-tracker :bind "set_spatial_context" :hash 2722037293)
 :void (spatial-context rid))

(defgmethod
 (open-xrspatial-entity-tracker+get-spatial-context :class
  'open-xrspatial-entity-tracker :bind "get_spatial_context" :hash 2944877500)
 rid)

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

(defgmethod
 (open-xrspatial-entity-tracker+get-next :class 'open-xrspatial-entity-tracker
  :bind "get_next" :hash 2798796760)
 open-xrstructure-base)

(defgmethod
 (open-xrspatial-entity-tracker+add-next :class 'open-xrspatial-entity-tracker
  :bind "add_next" :hash 334698771)
 :void (next open-xrstructure-base))

(defgmethod
 (open-xrspatial-entity-tracker+remove-next :class
  'open-xrspatial-entity-tracker :bind "remove_next" :hash 334698771)
 :void (next open-xrstructure-base))