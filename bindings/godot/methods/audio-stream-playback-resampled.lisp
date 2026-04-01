(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-playback-resampled+%mix-resampled :class
  'audio-stream-playback-resampled :bind "_mix_resampled" :hash 50157827
  :virtual common-lisp:t)
 int (dst-buffer (:pointer audio-frame)) (frame-count int))

(defgmethod
 (audio-stream-playback-resampled+%get-stream-sampling-rate :class
  'audio-stream-playback-resampled :bind "_get_stream_sampling_rate" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (audio-stream-playback-resampled+begin-resample :class
  'audio-stream-playback-resampled :bind "begin_resample" :hash 3218959716)
 :void)