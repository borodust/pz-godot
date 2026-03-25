(common-lisp:in-package :%godot)


(defgmethod
 (convert-transform-modifier-3d+set-apply-transform-mode :class
  'convert-transform-modifier-3d :bind "set_apply_transform_mode" :hash
  1386463405)
 :void (index int)
 (transform-mode convert-transform-modifier-3d+transform-mode))

(defgmethod
 (convert-transform-modifier-3d+get-apply-transform-mode :class
  'convert-transform-modifier-3d :bind "get_apply_transform_mode" :hash
  3234663511)
 convert-transform-modifier-3d+transform-mode (index int))

(defgmethod
 (convert-transform-modifier-3d+set-apply-axis :class
  'convert-transform-modifier-3d :bind "set_apply_axis" :hash 776736805)
 :void (index int) (axis vector-3+axis))

(defgmethod
 (convert-transform-modifier-3d+get-apply-axis :class
  'convert-transform-modifier-3d :bind "get_apply_axis" :hash 4131134770)
 vector-3+axis (index int))

(defgmethod
 (convert-transform-modifier-3d+set-apply-range-min :class
  'convert-transform-modifier-3d :bind "set_apply_range_min" :hash 1602489585)
 :void (index int) (range-min float))

(defgmethod
 (convert-transform-modifier-3d+get-apply-range-min :class
  'convert-transform-modifier-3d :bind "get_apply_range_min" :hash 2339986948)
 float (index int))

(defgmethod
 (convert-transform-modifier-3d+set-apply-range-max :class
  'convert-transform-modifier-3d :bind "set_apply_range_max" :hash 1602489585)
 :void (index int) (range-max float))

(defgmethod
 (convert-transform-modifier-3d+get-apply-range-max :class
  'convert-transform-modifier-3d :bind "get_apply_range_max" :hash 2339986948)
 float (index int))

(defgmethod
 (convert-transform-modifier-3d+set-reference-transform-mode :class
  'convert-transform-modifier-3d :bind "set_reference_transform_mode" :hash
  1386463405)
 :void (index int)
 (transform-mode convert-transform-modifier-3d+transform-mode))

(defgmethod
 (convert-transform-modifier-3d+get-reference-transform-mode :class
  'convert-transform-modifier-3d :bind "get_reference_transform_mode" :hash
  3234663511)
 convert-transform-modifier-3d+transform-mode (index int))

(defgmethod
 (convert-transform-modifier-3d+set-reference-axis :class
  'convert-transform-modifier-3d :bind "set_reference_axis" :hash 776736805)
 :void (index int) (axis vector-3+axis))

(defgmethod
 (convert-transform-modifier-3d+get-reference-axis :class
  'convert-transform-modifier-3d :bind "get_reference_axis" :hash 4131134770)
 vector-3+axis (index int))

(defgmethod
 (convert-transform-modifier-3d+set-reference-range-min :class
  'convert-transform-modifier-3d :bind "set_reference_range_min" :hash
  1602489585)
 :void (index int) (range-min float))

(defgmethod
 (convert-transform-modifier-3d+get-reference-range-min :class
  'convert-transform-modifier-3d :bind "get_reference_range_min" :hash
  2339986948)
 float (index int))

(defgmethod
 (convert-transform-modifier-3d+set-reference-range-max :class
  'convert-transform-modifier-3d :bind "set_reference_range_max" :hash
  1602489585)
 :void (index int) (range-max float))

(defgmethod
 (convert-transform-modifier-3d+get-reference-range-max :class
  'convert-transform-modifier-3d :bind "get_reference_range_max" :hash
  2339986948)
 float (index int))

(defgmethod
 (convert-transform-modifier-3d+set-relative :class
  'convert-transform-modifier-3d :bind "set_relative" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (convert-transform-modifier-3d+is-relative :class
  'convert-transform-modifier-3d :bind "is_relative" :hash 1116898809)
 bool (index int))

(defgmethod
 (convert-transform-modifier-3d+set-additive :class
  'convert-transform-modifier-3d :bind "set_additive" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (convert-transform-modifier-3d+is-additive :class
  'convert-transform-modifier-3d :bind "is_additive" :hash 1116898809)
 bool (index int))