(common-lisp:in-package :%godot)


(defgmethod
 (xrpose+set-has-tracking-data :class 'xrpose :bind "set_has_tracking_data"
  :hash 2586408642)
 :void (has-tracking-data bool))

(defgmethod
 (xrpose+get-has-tracking-data :class 'xrpose :bind "get_has_tracking_data"
  :hash 36873697)
 bool)

(defgmethod (xrpose+set-name :class 'xrpose :bind "set_name" :hash 3304788590)
 :void (name string-name))

(defgmethod (xrpose+get-name :class 'xrpose :bind "get_name" :hash 2002593661)
 string-name)

(defgmethod
 (xrpose+set-transform :class 'xrpose :bind "set_transform" :hash 2952846383)
 :void (transform transform-3d))

(defgmethod
 (xrpose+get-transform :class 'xrpose :bind "get_transform" :hash 3229777777)
 transform-3d)

(defgmethod
 (xrpose+get-adjusted-transform :class 'xrpose :bind "get_adjusted_transform"
  :hash 3229777777)
 transform-3d)

(defgmethod
 (xrpose+set-linear-velocity :class 'xrpose :bind "set_linear_velocity" :hash
  3460891852)
 :void (velocity vector-3))

(defgmethod
 (xrpose+get-linear-velocity :class 'xrpose :bind "get_linear_velocity" :hash
  3360562783)
 vector-3)

(defgmethod
 (xrpose+set-angular-velocity :class 'xrpose :bind "set_angular_velocity" :hash
  3460891852)
 :void (velocity vector-3))

(defgmethod
 (xrpose+get-angular-velocity :class 'xrpose :bind "get_angular_velocity" :hash
  3360562783)
 vector-3)

(defgmethod
 (xrpose+set-tracking-confidence :class 'xrpose :bind "set_tracking_confidence"
  :hash 4171656666)
 :void (tracking-confidence xrpose+tracking-confidence))

(defgmethod
 (xrpose+get-tracking-confidence :class 'xrpose :bind "get_tracking_confidence"
  :hash 2064923680)
 xrpose+tracking-confidence)