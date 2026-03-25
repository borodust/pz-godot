(common-lisp:in-package :%godot)


(defgproperty audio-stream-ogg-vorbis+packet-sequence 'audio-stream-ogg-vorbis
 :get 'audio-stream-ogg-vorbis+get-packet-sequence :set
 'audio-stream-ogg-vorbis+set-packet-sequence)

(defgproperty audio-stream-ogg-vorbis+bpm 'audio-stream-ogg-vorbis :get
 'audio-stream-ogg-vorbis+get-bpm :set 'audio-stream-ogg-vorbis+set-bpm)

(defgproperty audio-stream-ogg-vorbis+beat-count 'audio-stream-ogg-vorbis :get
 'audio-stream-ogg-vorbis+get-beat-count :set
 'audio-stream-ogg-vorbis+set-beat-count)

(defgproperty audio-stream-ogg-vorbis+bar-beats 'audio-stream-ogg-vorbis :get
 'audio-stream-ogg-vorbis+get-bar-beats :set
 'audio-stream-ogg-vorbis+set-bar-beats)

(defgproperty audio-stream-ogg-vorbis+tags 'audio-stream-ogg-vorbis :get
 'audio-stream-ogg-vorbis+get-tags :set 'audio-stream-ogg-vorbis+set-tags)

(defgproperty audio-stream-ogg-vorbis+loop 'audio-stream-ogg-vorbis :get
 'audio-stream-ogg-vorbis+has-loop :set 'audio-stream-ogg-vorbis+set-loop)

(defgproperty audio-stream-ogg-vorbis+loop-offset 'audio-stream-ogg-vorbis :get
 'audio-stream-ogg-vorbis+get-loop-offset :set
 'audio-stream-ogg-vorbis+set-loop-offset)