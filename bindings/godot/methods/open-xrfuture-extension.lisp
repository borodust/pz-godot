(common-lisp:in-package :%godot)


(defgmethod
 (open-xrfuture-extension+is-active :class 'open-xrfuture-extension :bind
  "is_active" :hash 36873697)
 bool)

(defgmethod
 (open-xrfuture-extension+register-future :class 'open-xrfuture-extension :bind
  "register_future" :hash 1038012256)
 open-xrfuture-result (future int) (on-success callable))

(defgmethod
 (open-xrfuture-extension+cancel-future :class 'open-xrfuture-extension :bind
  "cancel_future" :hash 1286410249)
 :void (future int))