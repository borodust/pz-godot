(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-record+set-recording-active :class 'audio-effect-record :bind
  "set_recording_active" :hash 2586408642)
 :void (record bool))

(defgmethod
 (audio-effect-record+is-recording-active :class 'audio-effect-record :bind
  "is_recording_active" :hash 36873697)
 bool)

(defgmethod
 (audio-effect-record+set-format :class 'audio-effect-record :bind "set_format"
  :hash 60648488)
 :void (format audio-stream-wav+format))

(defgmethod
 (audio-effect-record+get-format :class 'audio-effect-record :bind "get_format"
  :hash 3151724922)
 audio-stream-wav+format)

(defgmethod
 (audio-effect-record+get-recording :class 'audio-effect-record :bind
  "get_recording" :hash 2964110865)
 audio-stream-wav)