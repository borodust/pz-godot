(common-lisp:in-package :%godot)


(defgproperty audio-stream-randomizer+playback-mode 'audio-stream-randomizer
 :get 'audio-stream-randomizer+get-playback-mode :set
 'audio-stream-randomizer+set-playback-mode)

(defgproperty audio-stream-randomizer+random-pitch 'audio-stream-randomizer
 :get 'audio-stream-randomizer+get-random-pitch :set
 'audio-stream-randomizer+set-random-pitch)

(defgproperty audio-stream-randomizer+random-pitch-semitones
 'audio-stream-randomizer :get
 'audio-stream-randomizer+get-random-pitch-semitones :set
 'audio-stream-randomizer+set-random-pitch-semitones)

(defgproperty audio-stream-randomizer+random-volume-offset-db
 'audio-stream-randomizer :get
 'audio-stream-randomizer+get-random-volume-offset-db :set
 'audio-stream-randomizer+set-random-volume-offset-db)

(defgproperty audio-stream-randomizer+streams-count 'audio-stream-randomizer
 :get 'audio-stream-randomizer+get-streams-count :set
 'audio-stream-randomizer+set-streams-count)