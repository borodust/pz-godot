(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-synchronized+set-stream-count :class 'audio-stream-synchronized
  :bind "set_stream_count" :hash 1286410249)
 :void (stream-count int))

(defgmethod
 (audio-stream-synchronized+get-stream-count :class 'audio-stream-synchronized
  :bind "get_stream_count" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-synchronized+set-sync-stream :class 'audio-stream-synchronized
  :bind "set_sync_stream" :hash 111075094)
 :void (stream-index int) (audio-stream audio-stream))

(defgmethod
 (audio-stream-synchronized+get-sync-stream :class 'audio-stream-synchronized
  :bind "get_sync_stream" :hash 2739380747)
 audio-stream (stream-index int))

(defgmethod
 (audio-stream-synchronized+set-sync-stream-volume :class
  'audio-stream-synchronized :bind "set_sync_stream_volume" :hash 1602489585)
 :void (stream-index int) (volume-db float))

(defgmethod
 (audio-stream-synchronized+get-sync-stream-volume :class
  'audio-stream-synchronized :bind "get_sync_stream_volume" :hash 2339986948)
 float (stream-index int))