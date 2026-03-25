(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-playback+-start :class 'audio-stream-playback :bind "_start"
  :hash 373806689 :virtual common-lisp:t)
 :void (from-pos float))

(defgmethod
 (audio-stream-playback+-stop :class 'audio-stream-playback :bind "_stop" :hash
  3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (audio-stream-playback+-is-playing :class 'audio-stream-playback :bind
  "_is_playing" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (audio-stream-playback+-get-loop-count :class 'audio-stream-playback :bind
  "_get_loop_count" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (audio-stream-playback+-get-playback-position :class 'audio-stream-playback
  :bind "_get_playback_position" :hash 1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (audio-stream-playback+-seek :class 'audio-stream-playback :bind "_seek" :hash
  373806689 :virtual common-lisp:t)
 :void (position float))

(defgmethod
 (audio-stream-playback+-mix :class 'audio-stream-playback :bind "_mix" :hash
  925936155 :virtual common-lisp:t)
 int (buffer (:pointer audio-frame)) (rate-scale float) (frames int))

(defgmethod
 (audio-stream-playback+-tag-used-streams :class 'audio-stream-playback :bind
  "_tag_used_streams" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (audio-stream-playback+-set-parameter :class 'audio-stream-playback :bind
  "_set_parameter" :hash 3776071444 :virtual common-lisp:t)
 :void (name string-name) (value variant))

(defgmethod
 (audio-stream-playback+-get-parameter :class 'audio-stream-playback :bind
  "_get_parameter" :hash 2760726917 :virtual common-lisp:t)
 variant (name string-name))

(defgmethod
 (audio-stream-playback+set-sample-playback :class 'audio-stream-playback :bind
  "set_sample_playback" :hash 3195455091)
 :void (playback-sample audio-sample-playback))

(defgmethod
 (audio-stream-playback+get-sample-playback :class 'audio-stream-playback :bind
  "get_sample_playback" :hash 3482738536)
 audio-sample-playback)

(defgmethod
 (audio-stream-playback+mix-audio :class 'audio-stream-playback :bind
  "mix_audio" :hash 3341291446)
 packed-vector-2array (rate-scale float) (frames int))

(defgmethod
 (audio-stream-playback+start :class 'audio-stream-playback :bind "start" :hash
  1958160172)
 :void (from-pos float))

(defgmethod
 (audio-stream-playback+seek :class 'audio-stream-playback :bind "seek" :hash
  1958160172)
 :void (time float))

(defgmethod
 (audio-stream-playback+stop :class 'audio-stream-playback :bind "stop" :hash
  3218959716)
 :void)

(defgmethod
 (audio-stream-playback+get-loop-count :class 'audio-stream-playback :bind
  "get_loop_count" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-playback+get-playback-position :class 'audio-stream-playback
  :bind "get_playback_position" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-playback+is-playing :class 'audio-stream-playback :bind
  "is_playing" :hash 36873697)
 bool)