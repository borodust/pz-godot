(common-lisp:in-package :%godot)


(defgproperty audio-stream-mp3+data 'audio-stream-mp3 :get
 'audio-stream-mp3+get-data :set 'audio-stream-mp3+set-data)

(defgproperty audio-stream-mp3+bpm 'audio-stream-mp3 :get
 'audio-stream-mp3+get-bpm :set 'audio-stream-mp3+set-bpm)

(defgproperty audio-stream-mp3+beat-count 'audio-stream-mp3 :get
 'audio-stream-mp3+get-beat-count :set 'audio-stream-mp3+set-beat-count)

(defgproperty audio-stream-mp3+bar-beats 'audio-stream-mp3 :get
 'audio-stream-mp3+get-bar-beats :set 'audio-stream-mp3+set-bar-beats)

(defgproperty audio-stream-mp3+loop 'audio-stream-mp3 :get
 'audio-stream-mp3+has-loop :set 'audio-stream-mp3+set-loop)

(defgproperty audio-stream-mp3+loop-offset 'audio-stream-mp3 :get
 'audio-stream-mp3+get-loop-offset :set 'audio-stream-mp3+set-loop-offset)