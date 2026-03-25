(common-lisp:in-package :%godot)


(defgmethod
 (httpclient+connect-to-host :class 'httpclient :bind "connect_to_host" :hash
  504540374)
 error (host string) (port int) (tls-options tlsoptions))

(defgmethod
 (httpclient+set-connection :class 'httpclient :bind "set_connection" :hash
  3281897016)
 :void (connection stream-peer))

(defgmethod
 (httpclient+get-connection :class 'httpclient :bind "get_connection" :hash
  2741655269)
 stream-peer)

(defgmethod
 (httpclient+request-raw :class 'httpclient :bind "request_raw" :hash
  540161961)
 error (method httpclient+method) (url string) (headers packed-string-array)
 (body packed-byte-array))

(defgmethod
 (httpclient+request :class 'httpclient :bind "request" :hash 3778990155) error
 (method httpclient+method) (url string) (headers packed-string-array)
 (body string))

(defgmethod
 (httpclient+close :class 'httpclient :bind "close" :hash 3218959716) :void)

(defgmethod
 (httpclient+has-response :class 'httpclient :bind "has_response" :hash
  36873697)
 bool)

(defgmethod
 (httpclient+is-response-chunked :class 'httpclient :bind "is_response_chunked"
  :hash 36873697)
 bool)

(defgmethod
 (httpclient+get-response-code :class 'httpclient :bind "get_response_code"
  :hash 3905245786)
 int)

(defgmethod
 (httpclient+get-response-headers :class 'httpclient :bind
  "get_response_headers" :hash 2981934095)
 packed-string-array)

(defgmethod
 (httpclient+get-response-headers-as-dictionary :class 'httpclient :bind
  "get_response_headers_as_dictionary" :hash 2382534195)
 dictionary)

(defgmethod
 (httpclient+get-response-body-length :class 'httpclient :bind
  "get_response_body_length" :hash 3905245786)
 int)

(defgmethod
 (httpclient+read-response-body-chunk :class 'httpclient :bind
  "read_response_body_chunk" :hash 2115431945)
 packed-byte-array)

(defgmethod
 (httpclient+set-read-chunk-size :class 'httpclient :bind "set_read_chunk_size"
  :hash 1286410249)
 :void (bytes int))

(defgmethod
 (httpclient+get-read-chunk-size :class 'httpclient :bind "get_read_chunk_size"
  :hash 3905245786)
 int)

(defgmethod
 (httpclient+set-blocking-mode :class 'httpclient :bind "set_blocking_mode"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (httpclient+is-blocking-mode-enabled :class 'httpclient :bind
  "is_blocking_mode_enabled" :hash 36873697)
 bool)

(defgmethod
 (httpclient+get-status :class 'httpclient :bind "get_status" :hash 1426656811)
 httpclient+status)

(defgmethod (httpclient+poll :class 'httpclient :bind "poll" :hash 166280745)
 error)

(defgmethod
 (httpclient+set-http-proxy :class 'httpclient :bind "set_http_proxy" :hash
  2956805083)
 :void (host string) (port int))

(defgmethod
 (httpclient+set-https-proxy :class 'httpclient :bind "set_https_proxy" :hash
  2956805083)
 :void (host string) (port int))

(defgmethod
 (httpclient+query-string-from-dict :class 'httpclient :bind
  "query_string_from_dict" :hash 2538086567)
 string (fields dictionary))