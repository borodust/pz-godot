(common-lisp:in-package :%godot)


(defgproperty look-at-modifier-3d+target-node 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-target-node :set 'look-at-modifier-3d+set-target-node)

(defgproperty look-at-modifier-3d+bone-name 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-bone-name :set 'look-at-modifier-3d+set-bone-name)

(defgproperty look-at-modifier-3d+bone 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-bone :set 'look-at-modifier-3d+set-bone)

(defgproperty look-at-modifier-3d+forward-axis 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-forward-axis :set
 'look-at-modifier-3d+set-forward-axis)

(defgproperty look-at-modifier-3d+primary-rotation-axis 'look-at-modifier-3d
 :get 'look-at-modifier-3d+get-primary-rotation-axis :set
 'look-at-modifier-3d+set-primary-rotation-axis)

(defgproperty look-at-modifier-3d+use-secondary-rotation 'look-at-modifier-3d
 :get 'look-at-modifier-3d+is-using-secondary-rotation :set
 'look-at-modifier-3d+set-use-secondary-rotation)

(defgproperty look-at-modifier-3d+relative 'look-at-modifier-3d :get
 'look-at-modifier-3d+is-relative :set 'look-at-modifier-3d+set-relative)

(defgproperty look-at-modifier-3d+origin-from 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-origin-from :set 'look-at-modifier-3d+set-origin-from)

(defgproperty look-at-modifier-3d+origin-bone-name 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-origin-bone-name :set
 'look-at-modifier-3d+set-origin-bone-name)

(defgproperty look-at-modifier-3d+origin-bone 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-origin-bone :set 'look-at-modifier-3d+set-origin-bone)

(defgproperty look-at-modifier-3d+origin-external-node 'look-at-modifier-3d
 :get 'look-at-modifier-3d+get-origin-external-node :set
 'look-at-modifier-3d+set-origin-external-node)

(defgproperty look-at-modifier-3d+origin-offset 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-origin-offset :set
 'look-at-modifier-3d+set-origin-offset)

(defgproperty look-at-modifier-3d+origin-safe-margin 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-origin-safe-margin :set
 'look-at-modifier-3d+set-origin-safe-margin)

(defgproperty look-at-modifier-3d+duration 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-duration :set 'look-at-modifier-3d+set-duration)

(defgproperty look-at-modifier-3d+transition-type 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-transition-type :set
 'look-at-modifier-3d+set-transition-type)

(defgproperty look-at-modifier-3d+ease-type 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-ease-type :set 'look-at-modifier-3d+set-ease-type)

(defgproperty look-at-modifier-3d+use-angle-limitation 'look-at-modifier-3d
 :get 'look-at-modifier-3d+is-using-angle-limitation :set
 'look-at-modifier-3d+set-use-angle-limitation)

(defgproperty look-at-modifier-3d+symmetry-limitation 'look-at-modifier-3d :get
 'look-at-modifier-3d+is-limitation-symmetry :set
 'look-at-modifier-3d+set-symmetry-limitation)

(defgproperty look-at-modifier-3d+primary-limit-angle 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-primary-limit-angle :set
 'look-at-modifier-3d+set-primary-limit-angle)

(defgproperty look-at-modifier-3d+primary-damp-threshold 'look-at-modifier-3d
 :get 'look-at-modifier-3d+get-primary-damp-threshold :set
 'look-at-modifier-3d+set-primary-damp-threshold)

(defgproperty look-at-modifier-3d+primary-positive-limit-angle
 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-primary-positive-limit-angle :set
 'look-at-modifier-3d+set-primary-positive-limit-angle)

(defgproperty look-at-modifier-3d+primary-positive-damp-threshold
 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-primary-positive-damp-threshold :set
 'look-at-modifier-3d+set-primary-positive-damp-threshold)

(defgproperty look-at-modifier-3d+primary-negative-limit-angle
 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-primary-negative-limit-angle :set
 'look-at-modifier-3d+set-primary-negative-limit-angle)

(defgproperty look-at-modifier-3d+primary-negative-damp-threshold
 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-primary-negative-damp-threshold :set
 'look-at-modifier-3d+set-primary-negative-damp-threshold)

(defgproperty look-at-modifier-3d+secondary-limit-angle 'look-at-modifier-3d
 :get 'look-at-modifier-3d+get-secondary-limit-angle :set
 'look-at-modifier-3d+set-secondary-limit-angle)

(defgproperty look-at-modifier-3d+secondary-damp-threshold 'look-at-modifier-3d
 :get 'look-at-modifier-3d+get-secondary-damp-threshold :set
 'look-at-modifier-3d+set-secondary-damp-threshold)

(defgproperty look-at-modifier-3d+secondary-positive-limit-angle
 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-secondary-positive-limit-angle :set
 'look-at-modifier-3d+set-secondary-positive-limit-angle)

(defgproperty look-at-modifier-3d+secondary-positive-damp-threshold
 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-secondary-positive-damp-threshold :set
 'look-at-modifier-3d+set-secondary-positive-damp-threshold)

(defgproperty look-at-modifier-3d+secondary-negative-limit-angle
 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-secondary-negative-limit-angle :set
 'look-at-modifier-3d+set-secondary-negative-limit-angle)

(defgproperty look-at-modifier-3d+secondary-negative-damp-threshold
 'look-at-modifier-3d :get
 'look-at-modifier-3d+get-secondary-negative-damp-threshold :set
 'look-at-modifier-3d+set-secondary-negative-damp-threshold)