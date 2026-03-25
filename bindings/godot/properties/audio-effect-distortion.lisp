(common-lisp:in-package :%godot)


(defgproperty audio-effect-distortion+mode 'audio-effect-distortion :get
 'audio-effect-distortion+get-mode :set 'audio-effect-distortion+set-mode)

(defgproperty audio-effect-distortion+pre-gain 'audio-effect-distortion :get
 'audio-effect-distortion+get-pre-gain :set
 'audio-effect-distortion+set-pre-gain)

(defgproperty audio-effect-distortion+keep-hf-hz 'audio-effect-distortion :get
 'audio-effect-distortion+get-keep-hf-hz :set
 'audio-effect-distortion+set-keep-hf-hz)

(defgproperty audio-effect-distortion+drive 'audio-effect-distortion :get
 'audio-effect-distortion+get-drive :set 'audio-effect-distortion+set-drive)

(defgproperty audio-effect-distortion+post-gain 'audio-effect-distortion :get
 'audio-effect-distortion+get-post-gain :set
 'audio-effect-distortion+set-post-gain)