(common-lisp:in-package :%godot)


(defgmethod
 (audio-stream-interactive+set-clip-count :class 'audio-stream-interactive
  :bind "set_clip_count" :hash 1286410249)
 :void (clip-count int))

(defgmethod
 (audio-stream-interactive+get-clip-count :class 'audio-stream-interactive
  :bind "get_clip_count" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-interactive+set-initial-clip :class 'audio-stream-interactive
  :bind "set_initial_clip" :hash 1286410249)
 :void (clip-index int))

(defgmethod
 (audio-stream-interactive+get-initial-clip :class 'audio-stream-interactive
  :bind "get_initial_clip" :hash 3905245786)
 int)

(defgmethod
 (audio-stream-interactive+set-clip-name :class 'audio-stream-interactive :bind
  "set_clip_name" :hash 3780747571)
 :void (clip-index int) (name string-name))

(defgmethod
 (audio-stream-interactive+get-clip-name :class 'audio-stream-interactive :bind
  "get_clip_name" :hash 659327637)
 string-name (clip-index int))

(defgmethod
 (audio-stream-interactive+set-clip-stream :class 'audio-stream-interactive
  :bind "set_clip_stream" :hash 111075094)
 :void (clip-index int) (stream audio-stream))

(defgmethod
 (audio-stream-interactive+get-clip-stream :class 'audio-stream-interactive
  :bind "get_clip_stream" :hash 2739380747)
 audio-stream (clip-index int))

(defgmethod
 (audio-stream-interactive+set-clip-auto-advance :class
  'audio-stream-interactive :bind "set_clip_auto_advance" :hash 57217598)
 :void (clip-index int) (mode audio-stream-interactive+auto-advance-mode))

(defgmethod
 (audio-stream-interactive+get-clip-auto-advance :class
  'audio-stream-interactive :bind "get_clip_auto_advance" :hash 1778634807)
 audio-stream-interactive+auto-advance-mode (clip-index int))

(defgmethod
 (audio-stream-interactive+set-clip-auto-advance-next-clip :class
  'audio-stream-interactive :bind "set_clip_auto_advance_next_clip" :hash
  3937882851)
 :void (clip-index int) (auto-advance-next-clip int))

(defgmethod
 (audio-stream-interactive+get-clip-auto-advance-next-clip :class
  'audio-stream-interactive :bind "get_clip_auto_advance_next_clip" :hash
  923996154)
 int (clip-index int))

(defgmethod
 (audio-stream-interactive+add-transition :class 'audio-stream-interactive
  :bind "add_transition" :hash 1630280552)
 :void (from-clip int) (to-clip int)
 (from-time audio-stream-interactive+transition-from-time)
 (to-time audio-stream-interactive+transition-to-time)
 (fade-mode audio-stream-interactive+fade-mode) (fade-beats float)
 (use-filler-clip bool) (filler-clip int) (hold-previous bool))

(defgmethod
 (audio-stream-interactive+has-transition :class 'audio-stream-interactive
  :bind "has_transition" :hash 2522259332)
 bool (from-clip int) (to-clip int))

(defgmethod
 (audio-stream-interactive+erase-transition :class 'audio-stream-interactive
  :bind "erase_transition" :hash 3937882851)
 :void (from-clip int) (to-clip int))

(defgmethod
 (audio-stream-interactive+get-transition-list :class 'audio-stream-interactive
  :bind "get_transition_list" :hash 1930428628)
 packed-int-32array)

(defgmethod
 (audio-stream-interactive+get-transition-from-time :class
  'audio-stream-interactive :bind "get_transition_from_time" :hash 3453338158)
 audio-stream-interactive+transition-from-time (from-clip int) (to-clip int))

(defgmethod
 (audio-stream-interactive+get-transition-to-time :class
  'audio-stream-interactive :bind "get_transition_to_time" :hash 1369651373)
 audio-stream-interactive+transition-to-time (from-clip int) (to-clip int))

(defgmethod
 (audio-stream-interactive+get-transition-fade-mode :class
  'audio-stream-interactive :bind "get_transition_fade_mode" :hash 4065396087)
 audio-stream-interactive+fade-mode (from-clip int) (to-clip int))

(defgmethod
 (audio-stream-interactive+get-transition-fade-beats :class
  'audio-stream-interactive :bind "get_transition_fade_beats" :hash 3085491603)
 float (from-clip int) (to-clip int))

(defgmethod
 (audio-stream-interactive+is-transition-using-filler-clip :class
  'audio-stream-interactive :bind "is_transition_using_filler_clip" :hash
  2522259332)
 bool (from-clip int) (to-clip int))

(defgmethod
 (audio-stream-interactive+get-transition-filler-clip :class
  'audio-stream-interactive :bind "get_transition_filler_clip" :hash
  3175239445)
 int (from-clip int) (to-clip int))

(defgmethod
 (audio-stream-interactive+is-transition-holding-previous :class
  'audio-stream-interactive :bind "is_transition_holding_previous" :hash
  2522259332)
 bool (from-clip int) (to-clip int))