(common-lisp:in-package :%godot)


(defgmethod
 (hmaccontext+start :class 'hmaccontext :bind "start" :hash 3537364598) error
 (hash-type hashing-context+hash-type) (key packed-byte-array))

(defgmethod
 (hmaccontext+update :class 'hmaccontext :bind "update" :hash 680677267) error
 (data packed-byte-array))

(defgmethod
 (hmaccontext+finish :class 'hmaccontext :bind "finish" :hash 2115431945)
 packed-byte-array)