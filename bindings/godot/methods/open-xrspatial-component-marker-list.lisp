(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-component-marker-list+get-marker-type :class
  'open-xrspatial-component-marker-list :bind "get_marker_type" :hash
  2627847866)
 open-xrspatial-component-marker-list+marker-type (index int))

(defgmethod
 (open-xrspatial-component-marker-list+get-marker-id :class
  'open-xrspatial-component-marker-list :bind "get_marker_id" :hash 923996154)
 int (index int))

(defgmethod
 (open-xrspatial-component-marker-list+get-marker-data :class
  'open-xrspatial-component-marker-list :bind "get_marker_data" :hash
  4069510997)
 variant (snapshot rid) (index int))