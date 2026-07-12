(common-lisp:in-package :%godot)


(defgproperty rdhit-group+closest-hit-shader 'rdhit-group :get
 'rdhit-group+get-closest-hit-shader :set 'rdhit-group+set-closest-hit-shader)

(defgproperty rdhit-group+any-hit-shader 'rdhit-group :get
 'rdhit-group+get-any-hit-shader :set 'rdhit-group+set-any-hit-shader)

(defgproperty rdhit-group+intersection-shader 'rdhit-group :get
 'rdhit-group+get-intersection-shader :set 'rdhit-group+set-intersection-shader)