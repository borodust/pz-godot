(common-lisp:in-package :%godot)


(defgproperty tile-data+flip-h 'tile-data :get 'tile-data+get-flip-h :set
 'tile-data+set-flip-h)

(defgproperty tile-data+flip-v 'tile-data :get 'tile-data+get-flip-v :set
 'tile-data+set-flip-v)

(defgproperty tile-data+transpose 'tile-data :get 'tile-data+get-transpose :set
 'tile-data+set-transpose)

(defgproperty tile-data+texture-origin 'tile-data :get
 'tile-data+get-texture-origin :set 'tile-data+set-texture-origin)

(defgproperty tile-data+modulate 'tile-data :get 'tile-data+get-modulate :set
 'tile-data+set-modulate)

(defgproperty tile-data+material 'tile-data :get 'tile-data+get-material :set
 'tile-data+set-material)

(defgproperty tile-data+z-index 'tile-data :get 'tile-data+get-z-index :set
 'tile-data+set-z-index)

(defgproperty tile-data+y-sort-origin 'tile-data :get
 'tile-data+get-y-sort-origin :set 'tile-data+set-y-sort-origin)

(defgproperty tile-data+terrain-set 'tile-data :get 'tile-data+get-terrain-set
 :set 'tile-data+set-terrain-set)

(defgproperty tile-data+terrain 'tile-data :get 'tile-data+get-terrain :set
 'tile-data+set-terrain)

(defgproperty tile-data+probability 'tile-data :get 'tile-data+get-probability
 :set 'tile-data+set-probability)