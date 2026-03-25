(common-lisp:in-package :%godot)


(defgproperty audio-stream-wav+data 'audio-stream-wav :get
 'audio-stream-wav+get-data :set 'audio-stream-wav+set-data)

(defgproperty audio-stream-wav+format 'audio-stream-wav :get
 'audio-stream-wav+get-format :set 'audio-stream-wav+set-format)

(defgproperty audio-stream-wav+loop-mode 'audio-stream-wav :get
 'audio-stream-wav+get-loop-mode :set 'audio-stream-wav+set-loop-mode)

(defgproperty audio-stream-wav+loop-begin 'audio-stream-wav :get
 'audio-stream-wav+get-loop-begin :set 'audio-stream-wav+set-loop-begin)

(defgproperty audio-stream-wav+loop-end 'audio-stream-wav :get
 'audio-stream-wav+get-loop-end :set 'audio-stream-wav+set-loop-end)

(defgproperty audio-stream-wav+mix-rate 'audio-stream-wav :get
 'audio-stream-wav+get-mix-rate :set 'audio-stream-wav+set-mix-rate)

(defgproperty audio-stream-wav+stereo 'audio-stream-wav :get
 'audio-stream-wav+is-stereo :set 'audio-stream-wav+set-stereo)

(defgproperty audio-stream-wav+tags 'audio-stream-wav :get
 'audio-stream-wav+get-tags :set 'audio-stream-wav+set-tags)