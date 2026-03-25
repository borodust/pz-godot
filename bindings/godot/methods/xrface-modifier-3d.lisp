(common-lisp:in-package :%godot)


(defgmethod
 (xrface-modifier-3d+set-face-tracker :class 'xrface-modifier-3d :bind
  "set_face_tracker" :hash 3304788590)
 :void (tracker-name string-name))

(defgmethod
 (xrface-modifier-3d+get-face-tracker :class 'xrface-modifier-3d :bind
  "get_face_tracker" :hash 2002593661)
 string-name)

(defgmethod
 (xrface-modifier-3d+set-target :class 'xrface-modifier-3d :bind "set_target"
  :hash 1348162250)
 :void (target node-path))

(defgmethod
 (xrface-modifier-3d+get-target :class 'xrface-modifier-3d :bind "get_target"
  :hash 4075236667)
 node-path)