(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream+%instantiate-playback :class 'audio-stream :bind
  "_instantiate_playback" :hash 3093715447 :virtual common-lisp:t)
 audio-stream-playback)

(defgmethod
 (audio-stream+%get-stream-name :class 'audio-stream :bind "_get_stream_name"
  :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (audio-stream+%get-length :class 'audio-stream :bind "_get_length" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (audio-stream+%is-monophonic :class 'audio-stream :bind "_is_monophonic" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (audio-stream+%get-bpm :class 'audio-stream :bind "_get_bpm" :hash 1740695150
  :virtual common-lisp:t)
 float)

(defgmethod
 (audio-stream+%get-beat-count :class 'audio-stream :bind "_get_beat_count"
  :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (audio-stream+%get-tags :class 'audio-stream :bind "_get_tags" :hash
  3102165223 :virtual common-lisp:t)
 dictionary)

(defgmethod
 (audio-stream+%get-parameter-list :class 'audio-stream :bind
  "_get_parameter_list" :hash 3995934104 :virtual common-lisp:t)
 array)

(defgmethod
 (audio-stream+%has-loop :class 'audio-stream :bind "_has_loop" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (audio-stream+%get-bar-beats :class 'audio-stream :bind "_get_bar_beats" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (audio-stream+get-length :class 'audio-stream :bind "get_length" :hash
  1740695150)
 float)

(defgmethod
 (audio-stream+is-monophonic :class 'audio-stream :bind "is_monophonic" :hash
  36873697)
 bool)

(defgmethod
 (audio-stream+instantiate-playback :class 'audio-stream :bind
  "instantiate_playback" :hash 210135309)
 audio-stream-playback)

(defgmethod
 (audio-stream+can-be-sampled :class 'audio-stream :bind "can_be_sampled" :hash
  36873697)
 bool)

(defgmethod
 (audio-stream+generate-sample :class 'audio-stream :bind "generate_sample"
  :hash 2646048999)
 audio-sample)

(defgmethod
 (audio-stream+is-meta-stream :class 'audio-stream :bind "is_meta_stream" :hash
  36873697)
 bool)