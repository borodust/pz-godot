(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-wav+load-from-buffer :class 'audio-stream-wav :bind
  "load_from_buffer" :hash 4266838938 :static common-lisp:t)
 audio-stream-wav (stream-data packed-byte-array) (options dictionary))

(defgmethod
 (audio-stream-wav+load-from-file :class 'audio-stream-wav :bind
  "load_from_file" :hash 4015802384 :static common-lisp:t)
 audio-stream-wav (path string) (options dictionary))

(defgmethod
 (audio-stream-wav+set-data :class 'audio-stream-wav :bind "set_data" :hash
  2971499966)
 :void (data packed-byte-array))

(defgmethod
 (audio-stream-wav+get-data :class 'audio-stream-wav :bind "get_data" :hash
  2362200018)
 packed-byte-array)

(defgmethod
 (audio-stream-wav+set-format :class 'audio-stream-wav :bind "set_format" :hash
  60648488)
 :void (format audio-stream-wav+format))

(defgmethod
 (audio-stream-wav+get-format :class 'audio-stream-wav :bind "get_format" :hash
  3151724922)
 audio-stream-wav+format)

(defgmethod
 (audio-stream-wav+set-loop-mode :class 'audio-stream-wav :bind "set_loop_mode"
  :hash 2444882972)
 :void (loop-mode audio-stream-wav+loop-mode))

(defgmethod
 (audio-stream-wav+get-loop-mode :class 'audio-stream-wav :bind "get_loop_mode"
  :hash 393560655)
 audio-stream-wav+loop-mode)

(defgmethod
 (audio-stream-wav+set-loop-begin :class 'audio-stream-wav :bind
  "set_loop_begin" :hash 1286410249)
 :void (loop-begin int))

(defgmethod
 (audio-stream-wav+get-loop-begin :class 'audio-stream-wav :bind
  "get_loop_begin" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-wav+set-loop-end :class 'audio-stream-wav :bind "set_loop_end"
  :hash 1286410249)
 :void (loop-end int))

(defgmethod
 (audio-stream-wav+get-loop-end :class 'audio-stream-wav :bind "get_loop_end"
  :hash 3905245786)
 int)

(defgmethod
 (audio-stream-wav+set-mix-rate :class 'audio-stream-wav :bind "set_mix_rate"
  :hash 1286410249)
 :void (mix-rate int))

(defgmethod
 (audio-stream-wav+get-mix-rate :class 'audio-stream-wav :bind "get_mix_rate"
  :hash 3905245786)
 int)

(defgmethod
 (audio-stream-wav+set-stereo :class 'audio-stream-wav :bind "set_stereo" :hash
  2586408642)
 :void (stereo bool))

(defgmethod
 (audio-stream-wav+is-stereo :class 'audio-stream-wav :bind "is_stereo" :hash
  36873697)
 bool)

(defgmethod
 (audio-stream-wav+set-tags :class 'audio-stream-wav :bind "set_tags" :hash
  4155329257)
 :void (tags dictionary))

(defgmethod
 (audio-stream-wav+get-tags :class 'audio-stream-wav :bind "get_tags" :hash
  3102165223)
 dictionary)

(defgmethod
 (audio-stream-wav+save-to-wav :class 'audio-stream-wav :bind "save_to_wav"
  :hash 166001499)
 error (path string))