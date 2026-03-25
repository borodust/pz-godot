(common-lisp:in-package :%godot)


(defgmethod (signal+is-null :class 'signal :bind "is_null" :hash 3918633141)
 bool)

(defgmethod
 (signal+get-object :class 'signal :bind "get_object" :hash 4008621732) object)

(defgmethod
 (signal+get-object-id :class 'signal :bind "get_object_id" :hash 3173160232)
 int)

(defgmethod (signal+get-name :class 'signal :bind "get_name" :hash 1825232092)
 string-name)

(defgmethod (signal+connect :class 'signal :bind "connect" :hash 979702392) int
 (callable callable) (flags int))

(defgmethod
 (signal+disconnect :class 'signal :bind "disconnect" :hash 3470848906) :void
 (callable callable))

(defgmethod
 (signal+is-connected :class 'signal :bind "is_connected" :hash 4129521963)
 bool (callable callable))

(defgmethod
 (signal+get-connections :class 'signal :bind "get_connections" :hash
  4144163970)
 array)

(defgmethod
 (signal+has-connections :class 'signal :bind "has_connections" :hash
  3918633141)
 bool)

(defgmethod
 (signal+emit :class 'signal :bind "emit" :hash 3286317445 :vararg
  common-lisp:t)
 :void)