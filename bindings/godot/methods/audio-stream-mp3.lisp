(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-mp3+load-from-buffer :class 'audio-stream-mp3 :bind
  "load_from_buffer" :hash 1674970313 :static common-lisp:t)
 audio-stream-mp3 (stream-data packed-byte-array))

(defgmethod
 (audio-stream-mp3+load-from-file :class 'audio-stream-mp3 :bind
  "load_from_file" :hash 4238362998 :static common-lisp:t)
 audio-stream-mp3 (path string))

(defgmethod
 (audio-stream-mp3+set-data :class 'audio-stream-mp3 :bind "set_data" :hash
  2971499966)
 :void (data packed-byte-array))

(defgmethod
 (audio-stream-mp3+get-data :class 'audio-stream-mp3 :bind "get_data" :hash
  2362200018)
 packed-byte-array)

(defgmethod
 (audio-stream-mp3+set-loop :class 'audio-stream-mp3 :bind "set_loop" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (audio-stream-mp3+has-loop :class 'audio-stream-mp3 :bind "has_loop" :hash
  36873697)
 bool)

(defgmethod
 (audio-stream-mp3+set-loop-offset :class 'audio-stream-mp3 :bind
  "set_loop_offset" :hash 373806689)
 :void (seconds float))

(defgmethod
 (audio-stream-mp3+get-loop-offset :class 'audio-stream-mp3 :bind
  "get_loop_offset" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-mp3+set-bpm :class 'audio-stream-mp3 :bind "set_bpm" :hash
  373806689)
 :void (bpm float))

(defgmethod
 (audio-stream-mp3+get-bpm :class 'audio-stream-mp3 :bind "get_bpm" :hash
  1740695150)
 float)

(defgmethod
 (audio-stream-mp3+set-beat-count :class 'audio-stream-mp3 :bind
  "set_beat_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (audio-stream-mp3+get-beat-count :class 'audio-stream-mp3 :bind
  "get_beat_count" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-mp3+set-bar-beats :class 'audio-stream-mp3 :bind "set_bar_beats"
  :hash 1286410249)
 :void (count int))

(defgmethod
 (audio-stream-mp3+get-bar-beats :class 'audio-stream-mp3 :bind "get_bar_beats"
  :hash 3905245786)
 int)