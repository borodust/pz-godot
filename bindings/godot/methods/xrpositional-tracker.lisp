(common-lisp:in-package :%godot)


(defgmethod
 (xrpositional-tracker+get-tracker-profile :class 'xrpositional-tracker :bind
  "get_tracker_profile" :hash 201670096)
 string)

(defgmethod
 (xrpositional-tracker+set-tracker-profile :class 'xrpositional-tracker :bind
  "set_tracker_profile" :hash 83702148)
 :void (profile string))

(defgmethod
 (xrpositional-tracker+get-tracker-hand :class 'xrpositional-tracker :bind
  "get_tracker_hand" :hash 4181770860)
 xrpositional-tracker+tracker-hand)

(defgmethod
 (xrpositional-tracker+set-tracker-hand :class 'xrpositional-tracker :bind
  "set_tracker_hand" :hash 3904108980)
 :void (hand xrpositional-tracker+tracker-hand))

(defgmethod
 (xrpositional-tracker+has-pose :class 'xrpositional-tracker :bind "has_pose"
  :hash 2619796661)
 bool (name string-name))

(defgmethod
 (xrpositional-tracker+get-pose :class 'xrpositional-tracker :bind "get_pose"
  :hash 4099720006)
 xrpose (name string-name))

(defgmethod
 (xrpositional-tracker+invalidate-pose :class 'xrpositional-tracker :bind
  "invalidate_pose" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (xrpositional-tracker+set-pose :class 'xrpositional-tracker :bind "set_pose"
  :hash 3451230163)
 :void (name string-name) (transform transform-3d) (linear-velocity vector-3)
 (angular-velocity vector-3) (tracking-confidence xrpose+tracking-confidence))

(defgmethod
 (xrpositional-tracker+get-input :class 'xrpositional-tracker :bind "get_input"
  :hash 2760726917)
 variant (name string-name))

(defgmethod
 (xrpositional-tracker+set-input :class 'xrpositional-tracker :bind "set_input"
  :hash 3776071444)
 :void (name string-name) (value variant))