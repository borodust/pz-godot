(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-capture+can-get-buffer :class 'audio-effect-capture :bind
  "can_get_buffer" :hash 1116898809)
 bool (frames int))

(defgmethod
 (audio-effect-capture+get-buffer :class 'audio-effect-capture :bind
  "get_buffer" :hash 2649534757)
 packed-vector-2array (frames int))

(defgmethod
 (audio-effect-capture+clear-buffer :class 'audio-effect-capture :bind
  "clear_buffer" :hash 3218959716)
 :void)

(defgmethod
 (audio-effect-capture+set-buffer-length :class 'audio-effect-capture :bind
  "set_buffer_length" :hash 373806689)
 :void (buffer-length-seconds float))

(defgmethod
 (audio-effect-capture+get-buffer-length :class 'audio-effect-capture :bind
  "get_buffer_length" :hash 191475506)
 float)

(defgmethod
 (audio-effect-capture+get-frames-available :class 'audio-effect-capture :bind
  "get_frames_available" :hash 3905245786)
 int)

(defgmethod
 (audio-effect-capture+get-discarded-frames :class 'audio-effect-capture :bind
  "get_discarded_frames" :hash 3905245786)
 int)

(defgmethod
 (audio-effect-capture+get-buffer-length-frames :class 'audio-effect-capture
  :bind "get_buffer_length_frames" :hash 3905245786)
 int)

(defgmethod
 (audio-effect-capture+get-pushed-frames :class 'audio-effect-capture :bind
  "get_pushed_frames" :hash 3905245786)
 int)