(common-lisp:in-package :%godot)


(defgproperty httprequest+download-file 'httprequest :get
 'httprequest+get-download-file :set 'httprequest+set-download-file)

(defgproperty httprequest+download-chunk-size 'httprequest :get
 'httprequest+get-download-chunk-size :set 'httprequest+set-download-chunk-size)

(defgproperty httprequest+use-threads 'httprequest :get
 'httprequest+is-using-threads :set 'httprequest+set-use-threads)

(defgproperty httprequest+accept-gzip 'httprequest :get
 'httprequest+is-accepting-gzip :set 'httprequest+set-accept-gzip)

(defgproperty httprequest+body-size-limit 'httprequest :get
 'httprequest+get-body-size-limit :set 'httprequest+set-body-size-limit)

(defgproperty httprequest+max-redirects 'httprequest :get
 'httprequest+get-max-redirects :set 'httprequest+set-max-redirects)

(defgproperty httprequest+timeout 'httprequest :get 'httprequest+get-timeout
 :set 'httprequest+set-timeout)