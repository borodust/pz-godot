(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-randomizer+add-stream :class 'audio-stream-randomizer :bind
  "add_stream" :hash 1892018854)
 :void (index int) (stream audio-stream) (weight float))

(defgmethod
 (audio-stream-randomizer+move-stream :class 'audio-stream-randomizer :bind
  "move_stream" :hash 3937882851)
 :void (index-from int) (index-to int))

(defgmethod
 (audio-stream-randomizer+remove-stream :class 'audio-stream-randomizer :bind
  "remove_stream" :hash 1286410249)
 :void (index int))

(defgmethod
 (audio-stream-randomizer+set-stream :class 'audio-stream-randomizer :bind
  "set_stream" :hash 111075094)
 :void (index int) (stream audio-stream))

(defgmethod
 (audio-stream-randomizer+get-stream :class 'audio-stream-randomizer :bind
  "get_stream" :hash 2739380747)
 audio-stream (index int))

(defgmethod
 (audio-stream-randomizer+set-stream-probability-weight :class
  'audio-stream-randomizer :bind "set_stream_probability_weight" :hash
  1602489585)
 :void (index int) (weight float))

(defgmethod
 (audio-stream-randomizer+get-stream-probability-weight :class
  'audio-stream-randomizer :bind "get_stream_probability_weight" :hash
  2339986948)
 float (index int))

(defgmethod
 (audio-stream-randomizer+set-streams-count :class 'audio-stream-randomizer
  :bind "set_streams_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (audio-stream-randomizer+get-streams-count :class 'audio-stream-randomizer
  :bind "get_streams_count" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-randomizer+set-random-pitch :class 'audio-stream-randomizer
  :bind "set_random_pitch" :hash 373806689)
 :void (scale float))

(defgmethod
 (audio-stream-randomizer+get-random-pitch :class 'audio-stream-randomizer
  :bind "get_random_pitch" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-randomizer+set-random-pitch-semitones :class
  'audio-stream-randomizer :bind "set_random_pitch_semitones" :hash 373806689)
 :void (semitones float))

(defgmethod
 (audio-stream-randomizer+get-random-pitch-semitones :class
  'audio-stream-randomizer :bind "get_random_pitch_semitones" :hash 1740695150)
 float)

(defgmethod
 (audio-stream-randomizer+set-random-volume-offset-db :class
  'audio-stream-randomizer :bind "set_random_volume_offset_db" :hash 373806689)
 :void (db-offset float))

(defgmethod
 (audio-stream-randomizer+get-random-volume-offset-db :class
  'audio-stream-randomizer :bind "get_random_volume_offset_db" :hash
  1740695150)
 float)

(defgmethod
 (audio-stream-randomizer+set-playback-mode :class 'audio-stream-randomizer
  :bind "set_playback_mode" :hash 3950967023)
 :void (mode audio-stream-randomizer+playback-mode))

(defgmethod
 (audio-stream-randomizer+get-playback-mode :class 'audio-stream-randomizer
  :bind "get_playback_mode" :hash 3943055077)
 audio-stream-randomizer+playback-mode)