(common-lisp:in-package :%godot)


(defgmethod
 (video-stream+%instantiate-playback :class 'video-stream :bind
  "_instantiate_playback" :hash 294648086 :virtual common-lisp:t)
 video-stream-playback)

(defgmethod
 (video-stream+set-file :class 'video-stream :bind "set_file" :hash 83702148)
 :void (file string))

(defgmethod
 (video-stream+get-file :class 'video-stream :bind "get_file" :hash 2841200299)
 string)