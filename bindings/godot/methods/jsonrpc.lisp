(common-lisp:in-package :%godot)


(defgmethod
 (jsonrpc+set-method :class 'jsonrpc :bind "set_method" :hash 2137474292) :void
 (name string) (callback callable))

(defgmethod
 (jsonrpc+process-action :class 'jsonrpc :bind "process_action" :hash
  2963479484)
 variant (action variant) (recurse bool))

(defgmethod
 (jsonrpc+process-string :class 'jsonrpc :bind "process_string" :hash
  1703090593)
 string (action string))

(defgmethod
 (jsonrpc+make-request :class 'jsonrpc :bind "make_request" :hash 3423508980)
 dictionary (method string) (params variant) (id variant))

(defgmethod
 (jsonrpc+make-response :class 'jsonrpc :bind "make_response" :hash 5053918)
 dictionary (result variant) (id variant))

(defgmethod
 (jsonrpc+make-notification :class 'jsonrpc :bind "make_notification" :hash
  2949127017)
 dictionary (method string) (params variant))

(defgmethod
 (jsonrpc+make-response-error :class 'jsonrpc :bind "make_response_error" :hash
  928596297)
 dictionary (code int) (message string) (id variant))