(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-playback-interactive+switch-to-clip-by-name :class
  'audio-stream-playback-interactive :bind "switch_to_clip_by_name" :hash
  3304788590)
 :void (clip-name string-name))

(defgmethod
 (audio-stream-playback-interactive+switch-to-clip :class
  'audio-stream-playback-interactive :bind "switch_to_clip" :hash 1286410249)
 :void (clip-index int))

(defgmethod
 (audio-stream-playback-interactive+get-current-clip-index :class
  'audio-stream-playback-interactive :bind "get_current_clip_index" :hash
  3905245786)
 int)