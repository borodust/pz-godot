(common-lisp:in-package :%godot)


(defgproperty virtual-joystick+joystick-mode 'virtual-joystick :get
 'virtual-joystick+get-joystick-mode :set 'virtual-joystick+set-joystick-mode)

(defgproperty virtual-joystick+joystick-size 'virtual-joystick :get
 'virtual-joystick+get-joystick-size :set 'virtual-joystick+set-joystick-size)

(defgproperty virtual-joystick+tip-size 'virtual-joystick :get
 'virtual-joystick+get-tip-size :set 'virtual-joystick+set-tip-size)

(defgproperty virtual-joystick+deadzone-ratio 'virtual-joystick :get
 'virtual-joystick+get-deadzone-ratio :set 'virtual-joystick+set-deadzone-ratio)

(defgproperty virtual-joystick+clampzone-ratio 'virtual-joystick :get
 'virtual-joystick+get-clampzone-ratio :set
 'virtual-joystick+set-clampzone-ratio)

(defgproperty virtual-joystick+initial-offset-ratio 'virtual-joystick :get
 'virtual-joystick+get-initial-offset-ratio :set
 'virtual-joystick+set-initial-offset-ratio)

(defgproperty virtual-joystick+action-left 'virtual-joystick :get
 'virtual-joystick+get-action-left :set 'virtual-joystick+set-action-left)

(defgproperty virtual-joystick+action-right 'virtual-joystick :get
 'virtual-joystick+get-action-right :set 'virtual-joystick+set-action-right)

(defgproperty virtual-joystick+action-up 'virtual-joystick :get
 'virtual-joystick+get-action-up :set 'virtual-joystick+set-action-up)

(defgproperty virtual-joystick+action-down 'virtual-joystick :get
 'virtual-joystick+get-action-down :set 'virtual-joystick+set-action-down)

(defgproperty virtual-joystick+visibility-mode 'virtual-joystick :get
 'virtual-joystick+get-visibility-mode :set
 'virtual-joystick+set-visibility-mode)