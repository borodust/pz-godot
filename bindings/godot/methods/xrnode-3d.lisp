(common-lisp:in-package :%godot)


(defgmethod
 (xrnode-3d+set-tracker :class 'xrnode-3d :bind "set_tracker" :hash 3304788590)
 :void (tracker-name string-name))

(defgmethod
 (xrnode-3d+get-tracker :class 'xrnode-3d :bind "get_tracker" :hash 2002593661)
 string-name)

(defgmethod
 (xrnode-3d+set-pose-name :class 'xrnode-3d :bind "set_pose_name" :hash
  3304788590)
 :void (pose string-name))

(defgmethod
 (xrnode-3d+get-pose-name :class 'xrnode-3d :bind "get_pose_name" :hash
  2002593661)
 string-name)

(defgmethod
 (xrnode-3d+set-show-when-tracked :class 'xrnode-3d :bind
  "set_show_when_tracked" :hash 2586408642)
 :void (show bool))

(defgmethod
 (xrnode-3d+get-show-when-tracked :class 'xrnode-3d :bind
  "get_show_when_tracked" :hash 36873697)
 bool)

(defgmethod
 (xrnode-3d+get-is-active :class 'xrnode-3d :bind "get_is_active" :hash
  36873697)
 bool)

(defgmethod
 (xrnode-3d+get-has-tracking-data :class 'xrnode-3d :bind
  "get_has_tracking_data" :hash 36873697)
 bool)

(defgmethod
 (xrnode-3d+get-pose :class 'xrnode-3d :bind "get_pose" :hash 2806551826)
 xrpose)

(defgmethod
 (xrnode-3d+trigger-haptic-pulse :class 'xrnode-3d :bind "trigger_haptic_pulse"
  :hash 508576839)
 :void (action-name string) (frequency float) (amplitude float)
 (duration-sec float) (delay-sec float))