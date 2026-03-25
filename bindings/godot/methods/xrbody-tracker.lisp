(common-lisp:in-package :%godot)


(defgmethod
 (xrbody-tracker+set-has-tracking-data :class 'xrbody-tracker :bind
  "set_has_tracking_data" :hash 2586408642)
 :void (has-data bool))

(defgmethod
 (xrbody-tracker+get-has-tracking-data :class 'xrbody-tracker :bind
  "get_has_tracking_data" :hash 36873697)
 bool)

(defgmethod
 (xrbody-tracker+set-body-flags :class 'xrbody-tracker :bind "set_body_flags"
  :hash 2103235750)
 :void (flags xrbody-tracker+body-flags))

(defgmethod
 (xrbody-tracker+get-body-flags :class 'xrbody-tracker :bind "get_body_flags"
  :hash 3543166366)
 xrbody-tracker+body-flags)

(defgmethod
 (xrbody-tracker+set-joint-flags :class 'xrbody-tracker :bind "set_joint_flags"
  :hash 592144999)
 :void (joint xrbody-tracker+joint) (flags xrbody-tracker+joint-flags))

(defgmethod
 (xrbody-tracker+get-joint-flags :class 'xrbody-tracker :bind "get_joint_flags"
  :hash 1030162609)
 xrbody-tracker+joint-flags (joint xrbody-tracker+joint))

(defgmethod
 (xrbody-tracker+set-joint-transform :class 'xrbody-tracker :bind
  "set_joint_transform" :hash 2635424328)
 :void (joint xrbody-tracker+joint) (transform transform-3d))

(defgmethod
 (xrbody-tracker+get-joint-transform :class 'xrbody-tracker :bind
  "get_joint_transform" :hash 3474811534)
 transform-3d (joint xrbody-tracker+joint))