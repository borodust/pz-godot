(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-ogg-vorbis+load-from-buffer :class 'audio-stream-ogg-vorbis
  :bind "load_from_buffer" :hash 354904730 :static common-lisp:t)
 audio-stream-ogg-vorbis (stream-data packed-byte-array))

(defgmethod
 (audio-stream-ogg-vorbis+load-from-file :class 'audio-stream-ogg-vorbis :bind
  "load_from_file" :hash 797568536 :static common-lisp:t)
 audio-stream-ogg-vorbis (path string))

(defgmethod
 (audio-stream-ogg-vorbis+set-packet-sequence :class 'audio-stream-ogg-vorbis
  :bind "set_packet_sequence" :hash 438882457)
 :void (packet-sequence ogg-packet-sequence))

(defgmethod
 (audio-stream-ogg-vorbis+get-packet-sequence :class 'audio-stream-ogg-vorbis
  :bind "get_packet_sequence" :hash 2801636033)
 ogg-packet-sequence)

(defgmethod
 (audio-stream-ogg-vorbis+set-loop :class 'audio-stream-ogg-vorbis :bind
  "set_loop" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (audio-stream-ogg-vorbis+has-loop :class 'audio-stream-ogg-vorbis :bind
  "has_loop" :hash 36873697)
 bool)

(defgmethod
 (audio-stream-ogg-vorbis+set-loop-offset :class 'audio-stream-ogg-vorbis :bind
  "set_loop_offset" :hash 373806689)
 :void (seconds float))

(defgmethod
 (audio-stream-ogg-vorbis+get-loop-offset :class 'audio-stream-ogg-vorbis :bind
  "get_loop_offset" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-ogg-vorbis+set-bpm :class 'audio-stream-ogg-vorbis :bind
  "set_bpm" :hash 373806689)
 :void (bpm float))

(defgmethod
 (audio-stream-ogg-vorbis+get-bpm :class 'audio-stream-ogg-vorbis :bind
  "get_bpm" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-ogg-vorbis+set-beat-count :class 'audio-stream-ogg-vorbis :bind
  "set_beat_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (audio-stream-ogg-vorbis+get-beat-count :class 'audio-stream-ogg-vorbis :bind
  "get_beat_count" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-ogg-vorbis+set-bar-beats :class 'audio-stream-ogg-vorbis :bind
  "set_bar_beats" :hash 1286410249)
 :void (count int))

(defgmethod
 (audio-stream-ogg-vorbis+get-bar-beats :class 'audio-stream-ogg-vorbis :bind
  "get_bar_beats" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-ogg-vorbis+set-tags :class 'audio-stream-ogg-vorbis :bind
  "set_tags" :hash 4155329257)
 :void (tags dictionary))

(defgmethod
 (audio-stream-ogg-vorbis+get-tags :class 'audio-stream-ogg-vorbis :bind
  "get_tags" :hash 3102165223)
 dictionary)