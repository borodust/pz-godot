(common-lisp:in-package :%godot)


(defgmethod
 (stream-peer-extension+%get-data :class 'stream-peer-extension :bind
  "_get_data" :hash 298948178 :virtual common-lisp:t)
 error (r-buffer (:pointer :uint8)) (r-bytes int)
 (r-received (:pointer :int32)))

(defgmethod
 (stream-peer-extension+%get-partial-data :class 'stream-peer-extension :bind
  "_get_partial_data" :hash 298948178 :virtual common-lisp:t)
 error (r-buffer (:pointer :uint8)) (r-bytes int)
 (r-received (:pointer :int32)))

(defgmethod
 (stream-peer-extension+%put-data :class 'stream-peer-extension :bind
  "_put_data" :hash 298948178 :virtual common-lisp:t)
 error (p-data (:pointer :uint8)) (p-bytes int) (r-sent (:pointer :int32)))

(defgmethod
 (stream-peer-extension+%put-partial-data :class 'stream-peer-extension :bind
  "_put_partial_data" :hash 298948178 :virtual common-lisp:t)
 error (p-data (:pointer :uint8)) (p-bytes int) (r-sent (:pointer :int32)))

(defgmethod
 (stream-peer-extension+%get-available-bytes :class 'stream-peer-extension
  :bind "_get_available_bytes" :hash 3905245786 :virtual common-lisp:t)
 int)