(common-lisp:in-package :%godot)


(defgmethod
 (aescontext+start :class 'aescontext :bind "start" :hash 3122411423) error
 (mode aescontext+mode) (key packed-byte-array) (iv packed-byte-array))

(defgmethod
 (aescontext+update :class 'aescontext :bind "update" :hash 527836100)
 packed-byte-array (src packed-byte-array))

(defgmethod
 (aescontext+get-iv-state :class 'aescontext :bind "get_iv_state" :hash
  2115431945)
 packed-byte-array)

(defgmethod
 (aescontext+finish :class 'aescontext :bind "finish" :hash 3218959716) :void)