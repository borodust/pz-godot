(common-lisp:in-package :%godot)


(defgproperty audio-effect-compressor+threshold 'audio-effect-compressor :get
 'audio-effect-compressor+get-threshold :set
 'audio-effect-compressor+set-threshold)

(defgproperty audio-effect-compressor+ratio 'audio-effect-compressor :get
 'audio-effect-compressor+get-ratio :set 'audio-effect-compressor+set-ratio)

(defgproperty audio-effect-compressor+gain 'audio-effect-compressor :get
 'audio-effect-compressor+get-gain :set 'audio-effect-compressor+set-gain)

(defgproperty audio-effect-compressor+attack-us 'audio-effect-compressor :get
 'audio-effect-compressor+get-attack-us :set
 'audio-effect-compressor+set-attack-us)

(defgproperty audio-effect-compressor+release-ms 'audio-effect-compressor :get
 'audio-effect-compressor+get-release-ms :set
 'audio-effect-compressor+set-release-ms)

(defgproperty audio-effect-compressor+mix 'audio-effect-compressor :get
 'audio-effect-compressor+get-mix :set 'audio-effect-compressor+set-mix)

(defgproperty audio-effect-compressor+sidechain 'audio-effect-compressor :get
 'audio-effect-compressor+get-sidechain :set
 'audio-effect-compressor+set-sidechain)