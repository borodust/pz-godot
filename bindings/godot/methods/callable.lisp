(common-lisp:in-package :%godot)


(defgmethod
 (callable+create :class 'callable :bind "create" :hash 1709381114 :static
  common-lisp:t)
 callable (variant variant) (method string-name))

(defgmethod (callable+callv :class 'callable :bind "callv" :hash 413578926)
 variant (arguments array))

(defgmethod
 (callable+is-null :class 'callable :bind "is_null" :hash 3918633141) bool)

(defgmethod
 (callable+is-custom :class 'callable :bind "is_custom" :hash 3918633141) bool)

(defgmethod
 (callable+is-standard :class 'callable :bind "is_standard" :hash 3918633141)
 bool)

(defgmethod
 (callable+is-valid :class 'callable :bind "is_valid" :hash 3918633141) bool)

(defgmethod
 (callable+get-object :class 'callable :bind "get_object" :hash 4008621732)
 object)

(defgmethod
 (callable+get-object-id :class 'callable :bind "get_object_id" :hash
  3173160232)
 int)

(defgmethod
 (callable+get-method :class 'callable :bind "get_method" :hash 1825232092)
 string-name)

(defgmethod
 (callable+get-argument-count :class 'callable :bind "get_argument_count" :hash
  3173160232)
 int)

(defgmethod
 (callable+get-bound-arguments-count :class 'callable :bind
  "get_bound_arguments_count" :hash 3173160232)
 int)

(defgmethod
 (callable+get-bound-arguments :class 'callable :bind "get_bound_arguments"
  :hash 4144163970)
 array)

(defgmethod
 (callable+get-unbound-arguments-count :class 'callable :bind
  "get_unbound_arguments_count" :hash 3173160232)
 int)

(defgmethod (callable+hash :class 'callable :bind "hash" :hash 3173160232) int)

(defgmethod (callable+bindv :class 'callable :bind "bindv" :hash 3564560322)
 callable (arguments array))

(defgmethod (callable+unbind :class 'callable :bind "unbind" :hash 755001590)
 callable (argcount int))

(defgmethod
 (callable+call :class 'callable :bind "call" :hash 3643564216 :vararg
  common-lisp:t)
 variant)

(defgmethod
 (callable+call-deferred :class 'callable :bind "call_deferred" :hash
  3286317445 :vararg common-lisp:t)
 :void)

(defgmethod
 (callable+rpc :class 'callable :bind "rpc" :hash 3286317445 :vararg
  common-lisp:t)
 :void)

(defgmethod
 (callable+rpc-id :class 'callable :bind "rpc_id" :hash 2270047679 :vararg
  common-lisp:t)
 :void (peer-id int))

(defgmethod
 (callable+bind :class 'callable :bind "bind" :hash 3224143119 :vararg
  common-lisp:t)
 callable)