(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-component-data+-set-capacity :class
  'open-xrspatial-component-data :bind "_set_capacity" :hash 1286410249
  :virtual common-lisp:t)
 :void (capacity int))

(defgmethod
 (open-xrspatial-component-data+-get-component-type :class
  'open-xrspatial-component-data :bind "_get_component_type" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (open-xrspatial-component-data+-get-structure-data :class
  'open-xrspatial-component-data :bind "_get_structure_data" :hash 923996154
  :virtual common-lisp:t)
 int (next int))

(defgmethod
 (open-xrspatial-component-data+set-capacity :class
  'open-xrspatial-component-data :bind "set_capacity" :hash 1286410249)
 :void (capacity int))