(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-playlist+set-stream-count :class 'audio-stream-playlist :bind
  "set_stream_count" :hash 1286410249)
 :void (stream-count int))

(defgmethod
 (audio-stream-playlist+get-stream-count :class 'audio-stream-playlist :bind
  "get_stream_count" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-playlist+get-bpm :class 'audio-stream-playlist :bind "get_bpm"
  :hash 1740695150)
 float)

(defgmethod
 (audio-stream-playlist+set-list-stream :class 'audio-stream-playlist :bind
  "set_list_stream" :hash 111075094)
 :void (stream-index int) (audio-stream audio-stream))

(defgmethod
 (audio-stream-playlist+get-list-stream :class 'audio-stream-playlist :bind
  "get_list_stream" :hash 2739380747)
 audio-stream (stream-index int))

(defgmethod
 (audio-stream-playlist+set-shuffle :class 'audio-stream-playlist :bind
  "set_shuffle" :hash 2586408642)
 :void (shuffle bool))

(defgmethod
 (audio-stream-playlist+get-shuffle :class 'audio-stream-playlist :bind
  "get_shuffle" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-playlist+set-fade-time :class 'audio-stream-playlist :bind
  "set_fade_time" :hash 373806689)
 :void (dec float))

(defgmethod
 (audio-stream-playlist+get-fade-time :class 'audio-stream-playlist :bind
  "get_fade_time" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-playlist+set-loop :class 'audio-stream-playlist :bind "set_loop"
  :hash 2586408642)
 :void (loop bool))

(defgmethod
 (audio-stream-playlist+has-loop :class 'audio-stream-playlist :bind "has_loop"
  :hash 36873697)
 bool)