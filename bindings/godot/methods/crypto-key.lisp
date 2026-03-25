(common-lisp:in-package :%godot)


(defgmethod (crypto-key+save :class 'crypto-key :bind "save" :hash 885841341)
 error (path string) (public-only bool))

(defgmethod (crypto-key+load :class 'crypto-key :bind "load" :hash 885841341)
 error (path string) (public-only bool))

(defgmethod
 (crypto-key+is-public-only :class 'crypto-key :bind "is_public_only" :hash
  36873697)
 bool)

(defgmethod
 (crypto-key+save-to-string :class 'crypto-key :bind "save_to_string" :hash
  32795936)
 string (public-only bool))

(defgmethod
 (crypto-key+load-from-string :class 'crypto-key :bind "load_from_string" :hash
  885841341)
 error (string-key string) (public-only bool))