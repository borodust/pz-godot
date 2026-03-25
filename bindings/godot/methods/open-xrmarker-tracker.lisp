(common-lisp:in-package :%godot)


(defgmethod
 (open-xrmarker-tracker+set-bounds-size :class 'open-xrmarker-tracker :bind
  "set_bounds_size" :hash 743155724)
 :void (bounds-size vector-2))

(defgmethod
 (open-xrmarker-tracker+get-bounds-size :class 'open-xrmarker-tracker :bind
  "get_bounds_size" :hash 3341600327)
 vector-2)

(defgmethod
 (open-xrmarker-tracker+set-marker-type :class 'open-xrmarker-tracker :bind
  "set_marker_type" :hash 2156241362)
 :void (marker-type open-xrspatial-component-marker-list+marker-type))

(defgmethod
 (open-xrmarker-tracker+get-marker-type :class 'open-xrmarker-tracker :bind
  "get_marker_type" :hash 612702862)
 open-xrspatial-component-marker-list+marker-type)

(defgmethod
 (open-xrmarker-tracker+set-marker-id :class 'open-xrmarker-tracker :bind
  "set_marker_id" :hash 1286410249)
 :void (marker-id int))

(defgmethod
 (open-xrmarker-tracker+get-marker-id :class 'open-xrmarker-tracker :bind
  "get_marker_id" :hash 3905245786)
 int)

(defgmethod
 (open-xrmarker-tracker+set-marker-data :class 'open-xrmarker-tracker :bind
  "set_marker_data" :hash 1114965689)
 :void (marker-data variant))

(defgmethod
 (open-xrmarker-tracker+get-marker-data :class 'open-xrmarker-tracker :bind
  "get_marker_data" :hash 1214101251)
 variant)