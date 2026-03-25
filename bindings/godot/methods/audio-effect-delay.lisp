(common-lisp:in-package :%godot)


(defgmethod
 (audio-effect-delay+set-dry :class 'audio-effect-delay :bind "set_dry" :hash
  373806689)
 :void (amount float))

(defgmethod
 (audio-effect-delay+get-dry :class 'audio-effect-delay :bind "get_dry" :hash
  191475506)
 float)

(defgmethod
 (audio-effect-delay+set-tap1-active :class 'audio-effect-delay :bind
  "set_tap1_active" :hash 2586408642)
 :void (amount bool))

(defgmethod
 (audio-effect-delay+is-tap1-active :class 'audio-effect-delay :bind
  "is_tap1_active" :hash 36873697)
 bool)

(defgmethod
 (audio-effect-delay+set-tap1-delay-ms :class 'audio-effect-delay :bind
  "set_tap1_delay_ms" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-delay+get-tap1-delay-ms :class 'audio-effect-delay :bind
  "get_tap1_delay_ms" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-delay+set-tap1-level-db :class 'audio-effect-delay :bind
  "set_tap1_level_db" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-delay+get-tap1-level-db :class 'audio-effect-delay :bind
  "get_tap1_level_db" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-delay+set-tap1-pan :class 'audio-effect-delay :bind
  "set_tap1_pan" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-delay+get-tap1-pan :class 'audio-effect-delay :bind
  "get_tap1_pan" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-delay+set-tap2-active :class 'audio-effect-delay :bind
  "set_tap2_active" :hash 2586408642)
 :void (amount bool))

(defgmethod
 (audio-effect-delay+is-tap2-active :class 'audio-effect-delay :bind
  "is_tap2_active" :hash 36873697)
 bool)

(defgmethod
 (audio-effect-delay+set-tap2-delay-ms :class 'audio-effect-delay :bind
  "set_tap2_delay_ms" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-delay+get-tap2-delay-ms :class 'audio-effect-delay :bind
  "get_tap2_delay_ms" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-delay+set-tap2-level-db :class 'audio-effect-delay :bind
  "set_tap2_level_db" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-delay+get-tap2-level-db :class 'audio-effect-delay :bind
  "get_tap2_level_db" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-delay+set-tap2-pan :class 'audio-effect-delay :bind
  "set_tap2_pan" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-delay+get-tap2-pan :class 'audio-effect-delay :bind
  "get_tap2_pan" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-delay+set-feedback-active :class 'audio-effect-delay :bind
  "set_feedback_active" :hash 2586408642)
 :void (amount bool))

(defgmethod
 (audio-effect-delay+is-feedback-active :class 'audio-effect-delay :bind
  "is_feedback_active" :hash 36873697)
 bool)

(defgmethod
 (audio-effect-delay+set-feedback-delay-ms :class 'audio-effect-delay :bind
  "set_feedback_delay_ms" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-delay+get-feedback-delay-ms :class 'audio-effect-delay :bind
  "get_feedback_delay_ms" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-delay+set-feedback-level-db :class 'audio-effect-delay :bind
  "set_feedback_level_db" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-delay+get-feedback-level-db :class 'audio-effect-delay :bind
  "get_feedback_level_db" :hash 1740695150)
 float)

(defgmethod
 (audio-effect-delay+set-feedback-lowpass :class 'audio-effect-delay :bind
  "set_feedback_lowpass" :hash 373806689)
 :void (amount float))

(defgmethod
 (audio-effect-delay+get-feedback-lowpass :class 'audio-effect-delay :bind
  "get_feedback_lowpass" :hash 1740695150)
 float)