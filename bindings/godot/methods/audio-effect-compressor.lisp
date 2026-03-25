(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-compressor+set-threshold :class 'audio-effect-compressor :bind
  "set_threshold" :hash 373806689)
 :void (threshold float))

(defgmethod
 (audio-effect-compressor+get-threshold :class 'audio-effect-compressor :bind
  "get_threshold" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-compressor+set-ratio :class 'audio-effect-compressor :bind
  "set_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (audio-effect-compressor+get-ratio :class 'audio-effect-compressor :bind
  "get_ratio" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-compressor+set-gain :class 'audio-effect-compressor :bind
  "set_gain" :hash 373806689)
 :void (gain float))

(defgmethod
 (audio-effect-compressor+get-gain :class 'audio-effect-compressor :bind
  "get_gain" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-compressor+set-attack-us :class 'audio-effect-compressor :bind
  "set_attack_us" :hash 373806689)
 :void (attack-us float))

(defgmethod
 (audio-effect-compressor+get-attack-us :class 'audio-effect-compressor :bind
  "get_attack_us" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-compressor+set-release-ms :class 'audio-effect-compressor :bind
  "set_release_ms" :hash 373806689)
 :void (release-ms float))

(defgmethod
 (audio-effect-compressor+get-release-ms :class 'audio-effect-compressor :bind
  "get_release_ms" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-compressor+set-mix :class 'audio-effect-compressor :bind
  "set_mix" :hash 373806689)
 :void (mix float))

(defgmethod
 (audio-effect-compressor+get-mix :class 'audio-effect-compressor :bind
  "get_mix" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-compressor+set-sidechain :class 'audio-effect-compressor :bind
  "set_sidechain" :hash 3304788590)
 :void (sidechain string-name))

(defgmethod
 (audio-effect-compressor+get-sidechain :class 'audio-effect-compressor :bind
  "get_sidechain" :hash 2002593661)
 string-name)