(common-lisp:in-package :%godot)


(defgmethod
 (audio-listener-3d+make-current :class 'audio-listener-3d :bind "make_current"
  :hash 3218959716)
 :void)

(defgmethod
 (audio-listener-3d+clear-current :class 'audio-listener-3d :bind
  "clear_current" :hash 3218959716)
 :void)

(defgmethod
 (audio-listener-3d+is-current :class 'audio-listener-3d :bind "is_current"
  :hash 36873697)
 bool)

(defgmethod
 (audio-listener-3d+get-listener-transform :class 'audio-listener-3d :bind
  "get_listener_transform" :hash 3229777777)
 transform-3d)

(defgmethod
 (audio-listener-3d+set-doppler-tracking :class 'audio-listener-3d :bind
  "set_doppler_tracking" :hash 2365921740)
 :void (mode audio-listener-3d+doppler-tracking))

(defgmethod
 (audio-listener-3d+get-doppler-tracking :class 'audio-listener-3d :bind
  "get_doppler_tracking" :hash 550229039)
 audio-listener-3d+doppler-tracking)