(common-lisp:in-package :%godot)


(defgmethod
 (open-xranalog-threshold-modifier+set-on-threshold :class
  'open-xranalog-threshold-modifier :bind "set_on_threshold" :hash 373806689)
 :void (on-threshold float))

(defgmethod
 (open-xranalog-threshold-modifier+get-on-threshold :class
  'open-xranalog-threshold-modifier :bind "get_on_threshold" :hash 1740695150)
 float)

(defgmethod
 (open-xranalog-threshold-modifier+set-off-threshold :class
  'open-xranalog-threshold-modifier :bind "set_off_threshold" :hash 373806689)
 :void (off-threshold float))

(defgmethod
 (open-xranalog-threshold-modifier+get-off-threshold :class
  'open-xranalog-threshold-modifier :bind "get_off_threshold" :hash 1740695150)
 float)

(defgmethod
 (open-xranalog-threshold-modifier+set-on-haptic :class
  'open-xranalog-threshold-modifier :bind "set_on_haptic" :hash 2998020150)
 :void (haptic open-xrhaptic-base))

(defgmethod
 (open-xranalog-threshold-modifier+get-on-haptic :class
  'open-xranalog-threshold-modifier :bind "get_on_haptic" :hash 922310751)
 open-xrhaptic-base)

(defgmethod
 (open-xranalog-threshold-modifier+set-off-haptic :class
  'open-xranalog-threshold-modifier :bind "set_off_haptic" :hash 2998020150)
 :void (haptic open-xrhaptic-base))

(defgmethod
 (open-xranalog-threshold-modifier+get-off-haptic :class
  'open-xranalog-threshold-modifier :bind "get_off_haptic" :hash 922310751)
 open-xrhaptic-base)