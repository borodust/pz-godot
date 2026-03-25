(common-lisp:in-package :%godot)


(defgmethod
 (xrhand-tracker+set-has-tracking-data :class 'xrhand-tracker :bind
  "set_has_tracking_data" :hash 2586408642)
 :void (has-data bool))

(defgmethod
 (xrhand-tracker+get-has-tracking-data :class 'xrhand-tracker :bind
  "get_has_tracking_data" :hash 36873697)
 bool)

(defgmethod
 (xrhand-tracker+set-hand-tracking-source :class 'xrhand-tracker :bind
  "set_hand_tracking_source" :hash 2958308861)
 :void (source xrhand-tracker+hand-tracking-source))

(defgmethod
 (xrhand-tracker+get-hand-tracking-source :class 'xrhand-tracker :bind
  "get_hand_tracking_source" :hash 2475045250)
 xrhand-tracker+hand-tracking-source)

(defgmethod
 (xrhand-tracker+set-hand-joint-flags :class 'xrhand-tracker :bind
  "set_hand_joint_flags" :hash 3028437365)
 :void (joint xrhand-tracker+hand-joint)
 (flags xrhand-tracker+hand-joint-flags))

(defgmethod
 (xrhand-tracker+get-hand-joint-flags :class 'xrhand-tracker :bind
  "get_hand_joint_flags" :hash 1730972401)
 xrhand-tracker+hand-joint-flags (joint xrhand-tracker+hand-joint))

(defgmethod
 (xrhand-tracker+set-hand-joint-transform :class 'xrhand-tracker :bind
  "set_hand_joint_transform" :hash 2529959613)
 :void (joint xrhand-tracker+hand-joint) (transform transform-3d))

(defgmethod
 (xrhand-tracker+get-hand-joint-transform :class 'xrhand-tracker :bind
  "get_hand_joint_transform" :hash 1090840196)
 transform-3d (joint xrhand-tracker+hand-joint))

(defgmethod
 (xrhand-tracker+set-hand-joint-radius :class 'xrhand-tracker :bind
  "set_hand_joint_radius" :hash 2723659615)
 :void (joint xrhand-tracker+hand-joint) (radius float))

(defgmethod
 (xrhand-tracker+get-hand-joint-radius :class 'xrhand-tracker :bind
  "get_hand_joint_radius" :hash 3400025734)
 float (joint xrhand-tracker+hand-joint))

(defgmethod
 (xrhand-tracker+set-hand-joint-linear-velocity :class 'xrhand-tracker :bind
  "set_hand_joint_linear_velocity" :hash 1978646737)
 :void (joint xrhand-tracker+hand-joint) (linear-velocity vector-3))

(defgmethod
 (xrhand-tracker+get-hand-joint-linear-velocity :class 'xrhand-tracker :bind
  "get_hand_joint_linear_velocity" :hash 547240792)
 vector-3 (joint xrhand-tracker+hand-joint))

(defgmethod
 (xrhand-tracker+set-hand-joint-angular-velocity :class 'xrhand-tracker :bind
  "set_hand_joint_angular_velocity" :hash 1978646737)
 :void (joint xrhand-tracker+hand-joint) (angular-velocity vector-3))

(defgmethod
 (xrhand-tracker+get-hand-joint-angular-velocity :class 'xrhand-tracker :bind
  "get_hand_joint_angular_velocity" :hash 547240792)
 vector-3 (joint xrhand-tracker+hand-joint))