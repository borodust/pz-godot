(common-lisp:in-package :%godot)


(defgmethod
 (rdshader-source+set-stage-source :class 'rdshader-source :bind
  "set_stage_source" :hash 620821314)
 :void (stage rendering-device+shader-stage) (source string))

(defgmethod
 (rdshader-source+get-stage-source :class 'rdshader-source :bind
  "get_stage_source" :hash 3354920045)
 string (stage rendering-device+shader-stage))

(defgmethod
 (rdshader-source+set-language :class 'rdshader-source :bind "set_language"
  :hash 3422186742)
 :void (language rendering-device+shader-language))

(defgmethod
 (rdshader-source+get-language :class 'rdshader-source :bind "get_language"
  :hash 1063538261)
 rendering-device+shader-language)