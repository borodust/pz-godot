(common-lisp:in-package :%godot)


(defgmethod
 (movie-writer+%get-audio-mix-rate :class 'movie-writer :bind
  "_get_audio_mix_rate" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (movie-writer+%get-audio-speaker-mode :class 'movie-writer :bind
  "_get_audio_speaker_mode" :hash 2549190337 :virtual common-lisp:t)
 audio-server+speaker-mode)

(defgmethod
 (movie-writer+%handles-file :class 'movie-writer :bind "_handles_file" :hash
  3927539163 :virtual common-lisp:t)
 bool (path string))

(defgmethod
 (movie-writer+%write-begin :class 'movie-writer :bind "_write_begin" :hash
  1866453460 :virtual common-lisp:t)
 error (movie-size vector-2i) (fps int) (base-path string))

(defgmethod
 (movie-writer+%write-frame :class 'movie-writer :bind "_write_frame" :hash
  2784607037 :virtual common-lisp:t)
 error (frame-image image) (audio-frame-block (:pointer :void)))

(defgmethod
 (movie-writer+%write-end :class 'movie-writer :bind "_write_end" :hash
  3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (movie-writer+add-writer :class 'movie-writer :bind "add_writer" :hash
  4023702871 :static common-lisp:t)
 :void (writer movie-writer))