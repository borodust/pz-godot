(common-lisp:in-package :%godot)


(defgmethod
 (open-xrfuture-result+get-status :class 'open-xrfuture-result :bind
  "get_status" :hash 2023607463)
 open-xrfuture-result+result-status)

(defgmethod
 (open-xrfuture-result+get-future :class 'open-xrfuture-result :bind
  "get_future" :hash 3905245786)
 int)

(defgmethod
 (open-xrfuture-result+cancel-future :class 'open-xrfuture-result :bind
  "cancel_future" :hash 3218959716)
 :void)

(defgmethod
 (open-xrfuture-result+set-result-value :class 'open-xrfuture-result :bind
  "set_result_value" :hash 1114965689)
 :void (result-value variant))

(defgmethod
 (open-xrfuture-result+get-result-value :class 'open-xrfuture-result :bind
  "get_result_value" :hash 1214101251)
 variant)