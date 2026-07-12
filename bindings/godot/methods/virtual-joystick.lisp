(common-lisp:in-package :%godot)


(defgmethod
 (virtual-joystick+set-joystick-mode :class 'virtual-joystick :bind
  "set_joystick_mode" :hash 1316760817)
 :void (mode virtual-joystick+joystick-mode))

(defgmethod
 (virtual-joystick+get-joystick-mode :class 'virtual-joystick :bind
  "get_joystick_mode" :hash 2694680530)
 virtual-joystick+joystick-mode)

(defgmethod
 (virtual-joystick+set-joystick-size :class 'virtual-joystick :bind
  "set_joystick_size" :hash 373806689)
 :void (size float))

(defgmethod
 (virtual-joystick+get-joystick-size :class 'virtual-joystick :bind
  "get_joystick_size" :hash 1740695150)
 float)

(defgmethod
 (virtual-joystick+set-tip-size :class 'virtual-joystick :bind "set_tip_size"
  :hash 373806689)
 :void (size float))

(defgmethod
 (virtual-joystick+get-tip-size :class 'virtual-joystick :bind "get_tip_size"
  :hash 1740695150)
 float)

(defgmethod
 (virtual-joystick+set-deadzone-ratio :class 'virtual-joystick :bind
  "set_deadzone_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (virtual-joystick+get-deadzone-ratio :class 'virtual-joystick :bind
  "get_deadzone_ratio" :hash 1740695150)
 float)

(defgmethod
 (virtual-joystick+set-clampzone-ratio :class 'virtual-joystick :bind
  "set_clampzone_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (virtual-joystick+get-clampzone-ratio :class 'virtual-joystick :bind
  "get_clampzone_ratio" :hash 1740695150)
 float)

(defgmethod
 (virtual-joystick+set-initial-offset-ratio :class 'virtual-joystick :bind
  "set_initial_offset_ratio" :hash 743155724)
 :void (ratio vector-2))

(defgmethod
 (virtual-joystick+get-initial-offset-ratio :class 'virtual-joystick :bind
  "get_initial_offset_ratio" :hash 3341600327)
 vector-2)

(defgmethod
 (virtual-joystick+set-action-left :class 'virtual-joystick :bind
  "set_action_left" :hash 3304788590)
 :void (action string-name))

(defgmethod
 (virtual-joystick+get-action-left :class 'virtual-joystick :bind
  "get_action_left" :hash 2002593661)
 string-name)

(defgmethod
 (virtual-joystick+set-action-right :class 'virtual-joystick :bind
  "set_action_right" :hash 3304788590)
 :void (action string-name))

(defgmethod
 (virtual-joystick+get-action-right :class 'virtual-joystick :bind
  "get_action_right" :hash 2002593661)
 string-name)

(defgmethod
 (virtual-joystick+set-action-up :class 'virtual-joystick :bind "set_action_up"
  :hash 3304788590)
 :void (action string-name))

(defgmethod
 (virtual-joystick+get-action-up :class 'virtual-joystick :bind "get_action_up"
  :hash 2002593661)
 string-name)

(defgmethod
 (virtual-joystick+set-action-down :class 'virtual-joystick :bind
  "set_action_down" :hash 3304788590)
 :void (action string-name))

(defgmethod
 (virtual-joystick+get-action-down :class 'virtual-joystick :bind
  "get_action_down" :hash 2002593661)
 string-name)

(defgmethod
 (virtual-joystick+set-visibility-mode :class 'virtual-joystick :bind
  "set_visibility_mode" :hash 2638298545)
 :void (mode virtual-joystick+visibility-mode))

(defgmethod
 (virtual-joystick+get-visibility-mode :class 'virtual-joystick :bind
  "get_visibility_mode" :hash 3530872950)
 virtual-joystick+visibility-mode)