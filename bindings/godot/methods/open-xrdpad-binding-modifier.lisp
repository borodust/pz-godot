(common-lisp:in-package :%godot)


(defgmethod
 (open-xrdpad-binding-modifier+set-action-set :class
  'open-xrdpad-binding-modifier :bind "set_action_set" :hash 2093310581)
 :void (action-set open-xraction-set))

(defgmethod
 (open-xrdpad-binding-modifier+get-action-set :class
  'open-xrdpad-binding-modifier :bind "get_action_set" :hash 619941079)
 open-xraction-set)

(defgmethod
 (open-xrdpad-binding-modifier+set-input-path :class
  'open-xrdpad-binding-modifier :bind "set_input_path" :hash 83702148)
 :void (input-path string))

(defgmethod
 (open-xrdpad-binding-modifier+get-input-path :class
  'open-xrdpad-binding-modifier :bind "get_input_path" :hash 201670096)
 string)

(defgmethod
 (open-xrdpad-binding-modifier+set-threshold :class
  'open-xrdpad-binding-modifier :bind "set_threshold" :hash 373806689)
 :void (threshold float))

(defgmethod
 (open-xrdpad-binding-modifier+get-threshold :class
  'open-xrdpad-binding-modifier :bind "get_threshold" :hash 1740695150)
 float)

(defgmethod
 (open-xrdpad-binding-modifier+set-threshold-released :class
  'open-xrdpad-binding-modifier :bind "set_threshold_released" :hash 373806689)
 :void (threshold-released float))

(defgmethod
 (open-xrdpad-binding-modifier+get-threshold-released :class
  'open-xrdpad-binding-modifier :bind "get_threshold_released" :hash
  1740695150)
 float)

(defgmethod
 (open-xrdpad-binding-modifier+set-center-region :class
  'open-xrdpad-binding-modifier :bind "set_center_region" :hash 373806689)
 :void (center-region float))

(defgmethod
 (open-xrdpad-binding-modifier+get-center-region :class
  'open-xrdpad-binding-modifier :bind "get_center_region" :hash 1740695150)
 float)

(defgmethod
 (open-xrdpad-binding-modifier+set-wedge-angle :class
  'open-xrdpad-binding-modifier :bind "set_wedge_angle" :hash 373806689)
 :void (wedge-angle float))

(defgmethod
 (open-xrdpad-binding-modifier+get-wedge-angle :class
  'open-xrdpad-binding-modifier :bind "get_wedge_angle" :hash 1740695150)
 float)

(defgmethod
 (open-xrdpad-binding-modifier+set-is-sticky :class
  'open-xrdpad-binding-modifier :bind "set_is_sticky" :hash 2586408642)
 :void (is-sticky bool))

(defgmethod
 (open-xrdpad-binding-modifier+get-is-sticky :class
  'open-xrdpad-binding-modifier :bind "get_is_sticky" :hash 36873697)
 bool)

(defgmethod
 (open-xrdpad-binding-modifier+set-on-haptic :class
  'open-xrdpad-binding-modifier :bind "set_on_haptic" :hash 2998020150)
 :void (haptic open-xrhaptic-base))

(defgmethod
 (open-xrdpad-binding-modifier+get-on-haptic :class
  'open-xrdpad-binding-modifier :bind "get_on_haptic" :hash 922310751)
 open-xrhaptic-base)

(defgmethod
 (open-xrdpad-binding-modifier+set-off-haptic :class
  'open-xrdpad-binding-modifier :bind "set_off_haptic" :hash 2998020150)
 :void (haptic open-xrhaptic-base))

(defgmethod
 (open-xrdpad-binding-modifier+get-off-haptic :class
  'open-xrdpad-binding-modifier :bind "get_off_haptic" :hash 922310751)
 open-xrhaptic-base)