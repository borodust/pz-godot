(common-lisp:in-package :%godot)


(defgmethod
 (aim-modifier-3d+set-forward-axis :class 'aim-modifier-3d :bind
  "set_forward_axis" :hash 2496831085)
 :void (index int) (axis skeleton-modifier-3d+bone-axis))

(defgmethod
 (aim-modifier-3d+get-forward-axis :class 'aim-modifier-3d :bind
  "get_forward_axis" :hash 3949866735)
 skeleton-modifier-3d+bone-axis (index int))

(defgmethod
 (aim-modifier-3d+set-use-euler :class 'aim-modifier-3d :bind "set_use_euler"
  :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (aim-modifier-3d+is-using-euler :class 'aim-modifier-3d :bind "is_using_euler"
  :hash 1116898809)
 bool (index int))

(defgmethod
 (aim-modifier-3d+set-primary-rotation-axis :class 'aim-modifier-3d :bind
  "set_primary_rotation_axis" :hash 776736805)
 :void (index int) (axis vector-3+axis))

(defgmethod
 (aim-modifier-3d+get-primary-rotation-axis :class 'aim-modifier-3d :bind
  "get_primary_rotation_axis" :hash 4131134770)
 vector-3+axis (index int))

(defgmethod
 (aim-modifier-3d+set-use-secondary-rotation :class 'aim-modifier-3d :bind
  "set_use_secondary_rotation" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (aim-modifier-3d+is-using-secondary-rotation :class 'aim-modifier-3d :bind
  "is_using_secondary_rotation" :hash 1116898809)
 bool (index int))

(defgmethod
 (aim-modifier-3d+set-relative :class 'aim-modifier-3d :bind "set_relative"
  :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (aim-modifier-3d+is-relative :class 'aim-modifier-3d :bind "is_relative" :hash
  1116898809)
 bool (index int))