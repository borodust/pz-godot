(common-lisp:in-package :%godot)


(defgmethod
 (x509certificate+save :class 'x509certificate :bind "save" :hash 166001499)
 error (path string))

(defgmethod
 (x509certificate+load :class 'x509certificate :bind "load" :hash 166001499)
 error (path string))

(defgmethod
 (x509certificate+save-to-string :class 'x509certificate :bind "save_to_string"
  :hash 2841200299)
 string)

(defgmethod
 (x509certificate+load-from-string :class 'x509certificate :bind
  "load_from_string" :hash 166001499)
 error (string string))