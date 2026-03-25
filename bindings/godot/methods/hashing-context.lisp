(common-lisp:in-package :%godot)


(defgmethod
 (hashing-context+start :class 'hashing-context :bind "start" :hash 3940338335)
 error (type hashing-context+hash-type))

(defgmethod
 (hashing-context+update :class 'hashing-context :bind "update" :hash
  680677267)
 error (chunk packed-byte-array))

(defgmethod
 (hashing-context+finish :class 'hashing-context :bind "finish" :hash
  2115431945)
 packed-byte-array)