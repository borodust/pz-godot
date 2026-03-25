(common-lisp:in-package :%godot)


(defgmethod
 (ref-counted+init-ref :class 'ref-counted :bind "init_ref" :hash 2240911060)
 bool)

(defgmethod
 (ref-counted+reference :class 'ref-counted :bind "reference" :hash 2240911060)
 bool)

(defgmethod
 (ref-counted+unreference :class 'ref-counted :bind "unreference" :hash
  2240911060)
 bool)

(defgmethod
 (ref-counted+get-reference-count :class 'ref-counted :bind
  "get_reference_count" :hash 3905245786)
 int)