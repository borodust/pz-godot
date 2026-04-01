(common-lisp:in-package :%godot)


(defgmethod
 (video-stream-playback+%stop :class 'video-stream-playback :bind "_stop" :hash
  3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (video-stream-playback+%play :class 'video-stream-playback :bind "_play" :hash
  3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (video-stream-playback+%is-playing :class 'video-stream-playback :bind
  "_is_playing" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (video-stream-playback+%set-paused :class 'video-stream-playback :bind
  "_set_paused" :hash 2586408642 :virtual common-lisp:t)
 :void (paused bool))

(defgmethod
 (video-stream-playback+%is-paused :class 'video-stream-playback :bind
  "_is_paused" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (video-stream-playback+%get-length :class 'video-stream-playback :bind
  "_get_length" :hash 1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (video-stream-playback+%get-playback-position :class 'video-stream-playback
  :bind "_get_playback_position" :hash 1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (video-stream-playback+%seek :class 'video-stream-playback :bind "_seek" :hash
  373806689 :virtual common-lisp:t)
 :void (time float))

(defgmethod
 (video-stream-playback+%set-audio-track :class 'video-stream-playback :bind
  "_set_audio_track" :hash 1286410249 :virtual common-lisp:t)
 :void (idx int))

(defgmethod
 (video-stream-playback+%get-texture :class 'video-stream-playback :bind
  "_get_texture" :hash 3635182373 :virtual common-lisp:t)
 texture-2d)

(defgmethod
 (video-stream-playback+%update :class 'video-stream-playback :bind "_update"
  :hash 373806689 :virtual common-lisp:t)
 :void (delta float))

(defgmethod
 (video-stream-playback+%get-channels :class 'video-stream-playback :bind
  "_get_channels" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (video-stream-playback+%get-mix-rate :class 'video-stream-playback :bind
  "_get_mix_rate" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (video-stream-playback+mix-audio :class 'video-stream-playback :bind
  "mix_audio" :hash 93876830)
 int (num-frames int) (buffer packed-float-32array) (offset int))