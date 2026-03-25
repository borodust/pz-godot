(common-lisp:in-package :%godot)


(defgmethod
 (resource-importer-ogg-vorbis+load-from-buffer :class
  'resource-importer-ogg-vorbis :bind "load_from_buffer" :hash 354904730
  :static common-lisp:t)
 audio-stream-ogg-vorbis (stream-data packed-byte-array))

(defgmethod
 (resource-importer-ogg-vorbis+load-from-file :class
  'resource-importer-ogg-vorbis :bind "load_from_file" :hash 797568536 :static
  common-lisp:t)
 audio-stream-ogg-vorbis (path string))