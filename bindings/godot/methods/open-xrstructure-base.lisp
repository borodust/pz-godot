(common-lisp:in-package :%godot)


(defgmethod
 (open-xrstructure-base+%get-header :class 'open-xrstructure-base :bind
  "_get_header" :hash 3744713108 :virtual common-lisp:t)
 int (next int))

(defgmethod
 (open-xrstructure-base+get-structure-type :class 'open-xrstructure-base :bind
  "get_structure_type" :hash 2455072627)
 int)

(defgmethod
 (open-xrstructure-base+set-next :class 'open-xrstructure-base :bind "set_next"
  :hash 334698771)
 :void (entity open-xrstructure-base))

(defgmethod
 (open-xrstructure-base+get-next :class 'open-xrstructure-base :bind "get_next"
  :hash 2798796760)
 open-xrstructure-base)