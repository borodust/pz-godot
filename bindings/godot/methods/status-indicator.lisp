(common-lisp:in-package :%godot)


(defgmethod
 (status-indicator+set-tooltip :class 'status-indicator :bind "set_tooltip"
  :hash 83702148)
 :void (tooltip string))

(defgmethod
 (status-indicator+get-tooltip :class 'status-indicator :bind "get_tooltip"
  :hash 201670096)
 string)

(defgmethod
 (status-indicator+set-icon :class 'status-indicator :bind "set_icon" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (status-indicator+get-icon :class 'status-indicator :bind "get_icon" :hash
  3635182373)
 texture-2d)

(defgmethod
 (status-indicator+set-visible :class 'status-indicator :bind "set_visible"
  :hash 2586408642)
 :void (visible bool))

(defgmethod
 (status-indicator+is-visible :class 'status-indicator :bind "is_visible" :hash
  36873697)
 bool)

(defgmethod
 (status-indicator+set-menu :class 'status-indicator :bind "set_menu" :hash
  1348162250)
 :void (menu node-path))

(defgmethod
 (status-indicator+get-menu :class 'status-indicator :bind "get_menu" :hash
  4075236667)
 node-path)

(defgmethod
 (status-indicator+get-rect :class 'status-indicator :bind "get_rect" :hash
  1639390495)
 rect-2)