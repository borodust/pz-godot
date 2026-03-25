(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-distortion+set-mode :class 'audio-effect-distortion :bind
  "set_mode" :hash 1314744793)
 :void (mode audio-effect-distortion+mode))

(defgmethod
 (audio-effect-distortion+get-mode :class 'audio-effect-distortion :bind
  "get_mode" :hash 809118343)
 audio-effect-distortion+mode)

(defgmethod
 (audio-effect-distortion+set-pre-gain :class 'audio-effect-distortion :bind
  "set_pre_gain" :hash 373806689)
 :void (pre-gain float))

(defgmethod
 (audio-effect-distortion+get-pre-gain :class 'audio-effect-distortion :bind
  "get_pre_gain" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-distortion+set-keep-hf-hz :class 'audio-effect-distortion :bind
  "set_keep_hf_hz" :hash 373806689)
 :void (keep-hf-hz float))

(defgmethod
 (audio-effect-distortion+get-keep-hf-hz :class 'audio-effect-distortion :bind
  "get_keep_hf_hz" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-distortion+set-drive :class 'audio-effect-distortion :bind
  "set_drive" :hash 373806689)
 :void (drive float))

(defgmethod
 (audio-effect-distortion+get-drive :class 'audio-effect-distortion :bind
  "get_drive" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-distortion+set-post-gain :class 'audio-effect-distortion :bind
  "set_post_gain" :hash 373806689)
 :void (post-gain float))

(defgmethod
 (audio-effect-distortion+get-post-gain :class 'audio-effect-distortion :bind
  "get_post_gain" :hash 1740695150)
 float)