(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-polyphonic+set-polyphony :class 'audio-stream-polyphonic :bind
  "set_polyphony" :hash 1286410249)
 :void (voices int))

(defgmethod
 (audio-stream-polyphonic+get-polyphony :class 'audio-stream-polyphonic :bind
  "get_polyphony" :hash 3905245786)
 int)