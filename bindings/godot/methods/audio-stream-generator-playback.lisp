(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-generator-playback+push-frame :class
  'audio-stream-generator-playback :bind "push_frame" :hash 3975407249)
 bool (frame vector-2))

(defgmethod
 (audio-stream-generator-playback+can-push-buffer :class
  'audio-stream-generator-playback :bind "can_push_buffer" :hash 1116898809)
 bool (amount int))

(defgmethod
 (audio-stream-generator-playback+push-buffer :class
  'audio-stream-generator-playback :bind "push_buffer" :hash 1361156557)
 bool (frames packed-vector-2array))

(defgmethod
 (audio-stream-generator-playback+get-frames-available :class
  'audio-stream-generator-playback :bind "get_frames_available" :hash
  3905245786)
 int)

(defgmethod
 (audio-stream-generator-playback+get-skips :class
  'audio-stream-generator-playback :bind "get_skips" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-generator-playback+clear-buffer :class
  'audio-stream-generator-playback :bind "clear_buffer" :hash 3218959716)
 :void)