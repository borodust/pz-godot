(common-lisp:in-package :%godot)


(defgmethod
 (httprequest+request :class 'httprequest :bind "request" :hash 3215244323)
 error (url string) (custom-headers packed-string-array)
 (method httpclient+method) (request-data string))

(defgmethod
 (httprequest+request-raw :class 'httprequest :bind "request_raw" :hash
  2714829993)
 error (url string) (custom-headers packed-string-array)
 (method httpclient+method) (request-data-raw packed-byte-array))

(defgmethod
 (httprequest+cancel-request :class 'httprequest :bind "cancel_request" :hash
  3218959716)
 :void)

(defgmethod
 (httprequest+set-tls-options :class 'httprequest :bind "set_tls_options" :hash
  2210231844)
 :void (client-options tlsoptions))

(defgmethod
 (httprequest+get-http-client-status :class 'httprequest :bind
  "get_http_client_status" :hash 1426656811)
 httpclient+status)

(defgmethod
 (httprequest+set-use-threads :class 'httprequest :bind "set_use_threads" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (httprequest+is-using-threads :class 'httprequest :bind "is_using_threads"
  :hash 36873697)
 bool)

(defgmethod
 (httprequest+set-accept-gzip :class 'httprequest :bind "set_accept_gzip" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (httprequest+is-accepting-gzip :class 'httprequest :bind "is_accepting_gzip"
  :hash 36873697)
 bool)

(defgmethod
 (httprequest+set-body-size-limit :class 'httprequest :bind
  "set_body_size_limit" :hash 1286410249)
 :void (bytes int))

(defgmethod
 (httprequest+get-body-size-limit :class 'httprequest :bind
  "get_body_size_limit" :hash 3905245786)
 int)

(defgmethod
 (httprequest+set-max-redirects :class 'httprequest :bind "set_max_redirects"
  :hash 1286410249)
 :void (amount int))

(defgmethod
 (httprequest+get-max-redirects :class 'httprequest :bind "get_max_redirects"
  :hash 3905245786)
 int)

(defgmethod
 (httprequest+set-download-file :class 'httprequest :bind "set_download_file"
  :hash 83702148)
 :void (path string))

(defgmethod
 (httprequest+get-download-file :class 'httprequest :bind "get_download_file"
  :hash 201670096)
 string)

(defgmethod
 (httprequest+get-downloaded-bytes :class 'httprequest :bind
  "get_downloaded_bytes" :hash 3905245786)
 int)

(defgmethod
 (httprequest+get-body-size :class 'httprequest :bind "get_body_size" :hash
  3905245786)
 int)

(defgmethod
 (httprequest+set-timeout :class 'httprequest :bind "set_timeout" :hash
  373806689)
 :void (timeout float))

(defgmethod
 (httprequest+get-timeout :class 'httprequest :bind "get_timeout" :hash
  191475506)
 float)

(defgmethod
 (httprequest+set-download-chunk-size :class 'httprequest :bind
  "set_download_chunk_size" :hash 1286410249)
 :void (chunk-size int))

(defgmethod
 (httprequest+get-download-chunk-size :class 'httprequest :bind
  "get_download_chunk_size" :hash 3905245786)
 int)

(defgmethod
 (httprequest+set-http-proxy :class 'httprequest :bind "set_http_proxy" :hash
  2956805083)
 :void (host string) (port int))

(defgmethod
 (httprequest+set-https-proxy :class 'httprequest :bind "set_https_proxy" :hash
  2956805083)
 :void (host string) (port int))