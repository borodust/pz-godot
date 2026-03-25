(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-playback-polyphonic+play-stream :class
  'audio-stream-playback-polyphonic :bind "play_stream" :hash 1846744803)
 int (stream audio-stream) (from-offset float) (volume-db float)
 (pitch-scale float) (playback-type audio-server+playback-type)
 (bus string-name))

(defgmethod
 (audio-stream-playback-polyphonic+set-stream-volume :class
  'audio-stream-playback-polyphonic :bind "set_stream_volume" :hash 1602489585)
 :void (stream int) (volume-db float))

(defgmethod
 (audio-stream-playback-polyphonic+set-stream-pitch-scale :class
  'audio-stream-playback-polyphonic :bind "set_stream_pitch_scale" :hash
  1602489585)
 :void (stream int) (pitch-scale float))

(defgmethod
 (audio-stream-playback-polyphonic+is-stream-playing :class
  'audio-stream-playback-polyphonic :bind "is_stream_playing" :hash 1116898809)
 bool (stream int))

(defgmethod
 (audio-stream-playback-polyphonic+stop-stream :class
  'audio-stream-playback-polyphonic :bind "stop_stream" :hash 1286410249)
 :void (stream int))