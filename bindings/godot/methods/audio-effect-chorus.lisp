(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-chorus+set-voice-count :class 'audio-effect-chorus :bind
  "set_voice_count" :hash 1286410249)
 :void (voices int))

(defgmethod
 (audio-effect-chorus+get-voice-count :class 'audio-effect-chorus :bind
  "get_voice_count" :hash 3905245786)
 int)

(defgmethod
 (audio-effect-chorus+set-voice-delay-ms :class 'audio-effect-chorus :bind
  "set_voice_delay_ms" :hash 1602489585)
 :void (voice-idx int) (delay-ms float))

(defgmethod
 (audio-effect-chorus+get-voice-delay-ms :class 'audio-effect-chorus :bind
  "get_voice_delay_ms" :hash 2339986948)
 float (voice-idx int))

(defgmethod
 (audio-effect-chorus+set-voice-rate-hz :class 'audio-effect-chorus :bind
  "set_voice_rate_hz" :hash 1602489585)
 :void (voice-idx int) (rate-hz float))

(defgmethod
 (audio-effect-chorus+get-voice-rate-hz :class 'audio-effect-chorus :bind
  "get_voice_rate_hz" :hash 2339986948)
 float (voice-idx int))

(defgmethod
 (audio-effect-chorus+set-voice-depth-ms :class 'audio-effect-chorus :bind
  "set_voice_depth_ms" :hash 1602489585)
 :void (voice-idx int) (depth-ms float))

(defgmethod
 (audio-effect-chorus+get-voice-depth-ms :class 'audio-effect-chorus :bind
  "get_voice_depth_ms" :hash 2339986948)
 float (voice-idx int))

(defgmethod
 (audio-effect-chorus+set-voice-level-db :class 'audio-effect-chorus :bind
  "set_voice_level_db" :hash 1602489585)
 :void (voice-idx int) (level-db float))

(defgmethod
 (audio-effect-chorus+get-voice-level-db :class 'audio-effect-chorus :bind
  "get_voice_level_db" :hash 2339986948)
 float (voice-idx int))

(defgmethod
 (audio-effect-chorus+set-voice-cutoff-hz :class 'audio-effect-chorus :bind
  "set_voice_cutoff_hz" :hash 1602489585)
 :void (voice-idx int) (cutoff-hz float))

(defgmethod
 (audio-effect-chorus+get-voice-cutoff-hz :class 'audio-effect-chorus :bind
  "get_voice_cutoff_hz" :hash 2339986948)
 float (voice-idx int))

(defgmethod
 (audio-effect-chorus+set-voice-pan :class 'audio-effect-chorus :bind
  "set_voice_pan" :hash 1602489585)
 :void (voice-idx int) (pan float))

(defgmethod
 (audio-effect-chorus+get-voice-pan :class 'audio-effect-chorus :bind
  "get_voice_pan" :hash 2339986948)
 float (voice-idx int))

(defgmethod
 (audio-effect-chorus+set-wet :class 'audio-effect-chorus :bind "set_wet" :hash
  373806689)
 :void (amount float))

(defgmethod
 (audio-effect-chorus+get-wet :class 'audio-effect-chorus :bind "get_wet" :hash
  1740695150)
 float)

(defgmethod
 (audio-effect-chorus+set-dry :class 'audio-effect-chorus :bind "set_dry" :hash
  373806689)
 :void (amount float))

(defgmethod
 (audio-effect-chorus+get-dry :class 'audio-effect-chorus :bind "get_dry" :hash
  1740695150)
 float)